# ---------------------------------------------------------------------------
# Exploration: discover what can be queried.
#   agri_categories() -> the statistic categories on the landing page
#   agri_datasets()   -> the datasets (statistic tables) within a category
#   agri_dimensions() -> the classification axes + members of a dataset
# ---------------------------------------------------------------------------

# Names of the controls we interact with.
.agri_name_fieldgroup <- function() paste0(agri_ctl_prefix(), "lstFieldGroup")

# Find the postback link nodes on the landing page and return name + target.
#' List the statistic categories available on the landing page
#'
#' @param ses An \code{agri_session}. If NULL, a new session is opened.
#' @return A data frame with columns \code{name} (Chinese category name) and
#'   \code{target} (the internal postback id used to enter the category).
#' @export
agri_categories <- function(ses = NULL) {
  if (is.null(ses)) ses <- agri_session()
  if (!identical(ses$stage, "landing")) {
    # re-open a clean session so the landing links are present
    ses <- agri_session(verbose = ses$verbose)
  }
  doc <- .agri_doc(ses)
  anchors <- xml2::xml_find_all(doc, "//a[contains(@href, '__doPostBack')]")
  targets <- character(0); names_ <- character(0)
  for (a in anchors) {
    href <- xml2::xml_attr(a, "href")
    m <- regmatches(href, regexec("__doPostBack\\('([^']*)'", href))[[1]]
    if (length(m) < 2) next
    tgt <- m[2]
    if (!grepl("uctlInquireAdvance", tgt)) next
    txt <- trimws(xml2::xml_text(a))
    if (nchar(txt) == 0) next
    targets <- c(targets, tgt)
    names_  <- c(names_, txt)
  }
  df <- data.frame(name = names_, target = targets, stringsAsFactors = FALSE)
  df <- df[!duplicated(df$target), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# Resolve a category argument (name or target) to a postback target string.
.agri_resolve_category <- function(ses, category) {
  cats <- agri_categories(ses)
  if (category %in% cats$target) return(category)
  hit <- which(cats$name == category)
  if (length(hit) == 0) hit <- grep(category, cats$name, fixed = TRUE)
  if (length(hit) == 0) {
    stop("Category not found: '", category, "'. ",
         "Use agri_categories() to see valid names.", call. = FALSE)
  }
  cats$target[hit[1]]
}

# Enter a category: returns a session whose current page lists the datasets.
.agri_enter_category <- function(ses, category) {
  # always start from a fresh landing page so the category link exists
  if (!identical(ses$stage, "landing")) {
    ses <- agri_session(verbose = ses$verbose, timeout = ses$timeout)
  }
  target <- .agri_resolve_category(ses, category)
  ses <- .agri_submit(ses, event_target = target)
  ses$stage <- "datasets"
  ses
}

# Pull the dataset list (lstFieldGroup options) from the current page.
.agri_read_datasets <- function(ses) {
  doc <- .agri_doc(ses)
  sel <- xml2::xml_find_first(
    doc, paste0("//select[contains(@name,'lstFieldGroup')]"))
  if (inherits(sel, "xml_missing") || is.na(xml2::xml_name(sel))) {
    return(data.frame(id = character(0), name = character(0),
                      stringsAsFactors = FALSE))
  }
  opts <- xml2::xml_find_all(sel, ".//option")
  ids  <- vapply(opts, function(o) xml2::xml_attr(o, "value"), character(1))
  nm   <- vapply(opts, function(o) trimws(xml2::xml_text(o)), character(1))
  data.frame(id = ids, name = nm, stringsAsFactors = FALSE)
}

#' List the datasets (statistic tables) within a category
#'
#' @param category Category name or target (see \code{agri_categories}).
#' @param ses An optional existing \code{agri_session}.
#' @return A data frame with \code{id} and \code{name} of each dataset.
#' @export
agri_datasets <- function(category, ses = NULL) {
  if (is.null(ses)) ses <- agri_session()
  ses <- .agri_enter_category(ses, category)
  .agri_read_datasets(ses)
}

# Resolve a dataset argument (id or name) to its option value.
.agri_resolve_dataset <- function(ses, dataset) {
  ds <- .agri_read_datasets(ses)
  d <- as.character(dataset)
  if (d %in% ds$id) return(d)
  hit <- which(ds$name == d)
  if (length(hit) == 0) hit <- grep(d, ds$name, fixed = TRUE)
  if (length(hit) == 0) {
    stop("Dataset not found: '", dataset, "'. ",
         "Use agri_datasets(category) to see valid datasets.", call. = FALSE)
  }
  ds$id[hit[1]]
}

# Select a dataset (auto-postback on lstFieldGroup) so its dimension axes load.
.agri_select_dataset <- function(ses, dataset) {
  id <- .agri_resolve_dataset(ses, dataset)
  fg <- .agri_name_fieldgroup()
  ses <- .agri_submit(
    ses,
    overrides    = setNames(list(id), fg),
    event_target = fg
  )
  ses$stage <- "dimensions"
  attr(ses, "dataset_id") <- id
  ses
}

# Read the classification axes (one multi-select per 複分類) from current page.
.agri_read_dimensions <- function(ses) {
  doc <- .agri_doc(ses)
  sels <- xml2::xml_find_all(
    doc, "//select[contains(@name,'lstDimension')]")
  axes <- list()
  for (i in seq_along(sels)) {
    nd   <- sels[[i]]
    name <- xml2::xml_attr(nd, "name")
    opts <- xml2::xml_find_all(nd, ".//option")
    vals <- vapply(opts, function(o) xml2::xml_attr(o, "value"), character(1))
    labs <- vapply(opts, function(o) trimws(xml2::xml_text(o)), character(1))
    axes[[length(axes) + 1]] <- list(
      field = name,
      members = data.frame(label = labs, value = vals,
                           stringsAsFactors = FALSE)
    )
  }
  axes
}

#' Inspect the classification axes (dimensions) of a dataset
#'
#' A dataset is cross-classified by one or more axes (\dQuote{複分類}), for
#' example county, crop season and rice variety. This returns, for each axis,
#' the selectable members.
#'
#' @param category Category name or target.
#' @param dataset Dataset id or name (see \code{agri_datasets}).
#' @param ses An optional existing \code{agri_session}.
#' @return A named list; each element is a data frame of \code{label}/\code{value}
#'   members for one axis. The list is named \code{axis1}, \code{axis2}, ...
#' @export
agri_dimensions <- function(category, dataset, ses = NULL) {
  if (is.null(ses)) ses <- agri_session()
  ses <- .agri_enter_category(ses, category)
  ses <- .agri_select_dataset(ses, dataset)
  axes <- .agri_read_dimensions(ses)
  out <- lapply(axes, function(a) a$members)
  names(out) <- paste0("axis", seq_along(out))
  out
}
