# ---------------------------------------------------------------------------
# agri_fetch(): walk the full wizard and return the data table.
#
#   category -> dataset -> select classification members -> 查詢確認
#            -> set time range -> 查 詢 -> parse the HTML result table.
# ---------------------------------------------------------------------------

.agri_btn <- function(short) {
  # short: "query"  -> 查詢確認 (dataset/dimension page -> result page)
  #        "query2" -> 查 詢   (result page, re-query with new time range)
  switch(short,
    query  = list(name = paste0(agri_ctl_prefix(), "btnQuery"),  value = "查詢確認"),
    query2 = list(name = paste0(agri_ctl_prefix(), "btnQuery2"), value = "查 詢")
  )
}

# Decide which option values of one axis to submit, given a user spec.
.agri_pick_members <- function(members, spec) {
  if (is.null(spec) || (length(spec) == 1 &&
        is.character(spec) && tolower(spec) %in% c("all", "*"))) {
    return(members$value)
  }
  spec <- as.character(spec)
  out <- character(0)
  for (s in spec) {
    if (s %in% members$value) { out <- c(out, s); next }
    hit <- which(members$label == s)
    if (length(hit) == 0) hit <- grep(s, members$label, fixed = TRUE)
    if (length(hit) == 0) {
      stop("Classification member not found: '", s, "'. ",
           "Use agri_dimensions() to list valid members.", call. = FALSE)
    }
    out <- c(out, members$value[hit])
  }
  unique(out)
}

# Build the multi-select overrides for every axis from the `dimensions` arg.
.agri_dimension_overrides <- function(axes, dimensions) {
  ov <- list()
  for (i in seq_along(axes)) {
    spec <- NULL
    if (!is.null(dimensions)) {
      nm <- paste0("axis", i)
      if (!is.null(names(dimensions)) && nm %in% names(dimensions)) {
        spec <- dimensions[[nm]]
      } else if (i <= length(dimensions)) {
        spec <- dimensions[[i]]
      }
    }
    vals <- .agri_pick_members(axes[[i]]$members, spec)
    ov[[axes[[i]]$field]] <- vals
  }
  ov
}

# Find the date-range <select> controls present on the result page.
.agri_date_fields <- function(ses) {
  doc <- .agri_doc(ses)
  sels <- xml2::xml_find_all(doc, "//select[@name]")
  info <- list(begin = NULL, end = NULL)
  for (nd in sels) {
    name <- xml2::xml_attr(nd, "name")
    opts <- xml2::xml_find_all(nd, ".//option")
    if (length(opts) == 0) next
    vals <- vapply(opts, function(o) xml2::xml_attr(o, "value"), character(1))
    if (grepl("Begin", name, ignore.case = TRUE) &&
        grepl("Year|Date|Month|Season|Time", name, ignore.case = TRUE)) {
      info$begin <- list(field = name, values = vals)
    } else if (grepl("End", name, ignore.case = TRUE) &&
        grepl("Year|Date|Month|Season|Time", name, ignore.case = TRUE)) {
      info$end <- list(field = name, values = vals)
    }
  }
  info
}

#' Fetch a statistics table as a tidy data frame
#'
#' Drives the entire query wizard and downloads the resulting table. By
#' default every classification member on every axis and the full available
#' time range are selected, i.e. you get everything the dataset offers.
#'
#' @param category Category name or target (see \code{agri_categories}).
#' @param dataset Dataset id or name (see \code{agri_datasets}).
#' @param dimensions Optional control over which classification members to
#'   include. Either \code{NULL} (all members of all axes) or a list with one
#'   element per axis (positional, or named \code{axis1}, \code{axis2}, ...).
#'   Each element is a character vector of member labels (or values), or the
#'   string \code{"all"}.
#' @param year_begin,year_end Optional start/end of the time range. Accepts the
#'   ROC year as a number or string (e.g. 110 or "110"). When \code{NULL} the
#'   full available range is used.
#' @param tidy If TRUE (default) the table is returned in long/tidy form with
#'   columns \code{period}, \code{year_roc}, \code{year_ad}, \code{series} and
#'   \code{value}. If FALSE a wide data frame (period + one column per series)
#'   is returned.
#' @param raw If TRUE, returns the raw parsed grid (character matrix) without
#'   any tidying; useful for debugging unusual tables.
#' @param ses An optional existing \code{agri_session} (a new one is created if
#'   omitted).
#' @return A data frame. Metadata is attached as attributes \code{title},
#'   \code{category}, \code{dataset} and \code{fetched_at}.
#' @export
agri_fetch <- function(category, dataset, dimensions = NULL,
                       year_begin = NULL, year_end = NULL,
                       tidy = TRUE, raw = FALSE, ses = NULL) {
  if (is.null(ses)) ses <- agri_session()

  ses <- .agri_enter_category(ses, category)
  ses <- .agri_select_dataset(ses, dataset)
  dataset_id <- attr(ses, "dataset_id")

  axes <- .agri_read_dimensions(ses)
  if (length(axes) == 0) {
    stop("No classification axes found for this dataset.", call. = FALSE)
  }
  ov <- .agri_dimension_overrides(axes, dimensions)

  if (ses$verbose) message("Submitting query (",
    paste(vapply(ov, length, integer(1)), collapse = "x"), " members)...")
  ses <- .agri_submit(ses, overrides = ov, button = .agri_btn("query"),
                      event_target = "")
  ses$stage <- "result"

  # time range
  dt <- .agri_date_fields(ses)
  if (!is.null(dt$begin) && !is.null(dt$end)) {
    bvals <- dt$begin$values; evals <- dt$end$values
    bsel <- if (is.null(year_begin)) bvals[1] else as.character(year_begin)
    esel <- if (is.null(year_end))   evals[length(evals)] else as.character(year_end)
    if (!bsel %in% bvals) {
      stop("year_begin '", bsel, "' not available. Options: ",
           paste(bvals, collapse = ", "), call. = FALSE)
    }
    if (!esel %in% evals) {
      stop("year_end '", esel, "' not available. Options: ",
           paste(evals, collapse = ", "), call. = FALSE)
    }
    ov2 <- setNames(list(bsel, esel), c(dt$begin$field, dt$end$field))
    if (ses$verbose) message("Re-querying time range ", bsel, " ~ ", esel, "...")
    ses <- .agri_submit(ses, overrides = ov2, button = .agri_btn("query2"),
                        event_target = "")
  }

  parsed <- .agri_parse_result(ses$html, tidy = tidy, raw = raw)
  attr(parsed, "category")   <- category
  attr(parsed, "dataset")    <- dataset
  attr(parsed, "dataset_id") <- dataset_id
  attr(parsed, "fetched_at") <- Sys.time()
  parsed
}
