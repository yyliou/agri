# ---------------------------------------------------------------------------
# Parsing the HTML result table into a data frame.
#
# The result grid is rendered as a leaf <table> (no nested tables) with a few
# header rows that use colspan/rowspan for multi-level classification headers,
# followed by one row per time period whose first cell looks like "111年" (or
# "111年01月" for monthly data). We:
#   * locate that leaf table,
#   * expand it to a rectangle with rvest::html_table(fill = TRUE),
#   * split header rows from data rows,
#   * build a column name per value column from the header rows,
#   * coerce values to numbers and (optionally) reshape to long form.
# ---------------------------------------------------------------------------

# Period pattern: leading ROC year, e.g. 111年, 111年01月, 111年第1季.
.agri_period_rx <- "^[0-9]{2,4}\\s*年"

.agri_html_grid <- function(node) {
  # rvest::html_table() expands rowspan/colspan (fill). Keep cells as raw
  # strings: convert = FALSE prevents numbers with thousands separators from
  # being silently turned into NA. Both args vary across rvest versions, so
  # fall back gracefully.
  attempts <- list(
    function() rvest::html_table(node, header = FALSE, trim = TRUE, convert = FALSE),
    function() rvest::html_table(node, header = FALSE, trim = TRUE, fill = TRUE),
    function() rvest::html_table(node, header = FALSE, trim = TRUE),
    function() rvest::html_table(node)
  )
  g <- NULL
  for (f in attempts) {
    g <- tryCatch(suppressWarnings(f()), error = function(e) NULL)
    if (!is.null(g)) break
  }
  if (is.null(g)) return(NULL)
  m <- as.matrix(g)
  storage.mode(m) <- "character"
  m[is.na(m)] <- ""
  m <- trimws(m)
  m
}

# Choose the leaf table that looks like the data grid.
.agri_find_data_grid <- function(doc) {
  leaves <- xml2::xml_find_all(doc, "//table[not(.//table)]")
  best <- NULL; best_score <- 0
  for (nd in leaves) {
    m <- .agri_html_grid(nd)
    if (is.null(m) || nrow(m) < 2 || ncol(m) < 2) next
    score <- sum(grepl(.agri_period_rx, m[, 1]))
    if (score > best_score) { best_score <- score; best <- m }
  }
  if (is.null(best)) {
    # fallback: leaf table with the most numeric-looking cells
    for (nd in leaves) {
      m <- .agri_html_grid(nd)
      if (is.null(m)) next
      score <- sum(grepl("^[0-9][0-9,\\.]*$", m))
      if (score > best_score) { best_score <- score; best <- m }
    }
  }
  best
}

# Build a column name for each column from the header rows.
.agri_build_names <- function(header, ncol_total) {
  if (is.null(header) || nrow(header) == 0) {
    return(paste0("V", seq_len(ncol_total)))
  }
  # Drop "title/banner" rows: rows whose non-empty cells are all identical
  # (these come from a single colspan cell duplicated across the row).
  keep <- logical(nrow(header))
  for (r in seq_len(nrow(header))) {
    vals <- header[r, ]
    nz <- vals[nzchar(vals)]
    keep[r] <- !(length(unique(nz)) <= 1 && length(nz) > 1)
  }
  naming <- header[keep, , drop = FALSE]
  nms <- character(ncol_total)
  for (j in seq_len(ncol_total)) {
    if (nrow(naming) == 0) { nms[j] <- paste0("V", j); next }
    cells <- naming[, j]
    cells <- cells[nzchar(cells)]
    cells <- cells[!duplicated(cells)]
    nms[j] <- if (length(cells)) paste(cells, collapse = "_") else paste0("V", j)
  }
  nms
}

.agri_to_number <- function(x) {
  x <- gsub(",", "", x, fixed = TRUE)
  x <- trimws(x)
  x[x %in% c("-", "...", "…", "x", "X", "")] <- NA
  suppressWarnings(as.numeric(x))
}

# Extract the dataset title / unit string from the grid (a banner header row).
.agri_grid_title <- function(grid, first_data) {
  if (first_data <= 1) return(NA_character_)
  header <- grid[seq_len(first_data - 1), , drop = FALSE]
  for (r in seq_len(nrow(header))) {
    nz <- header[r, ][nzchar(header[r, ])]
    if (length(unique(nz)) == 1 && length(nz) >= 1) return(unique(nz))
  }
  NA_character_
}

.agri_parse_result <- function(html_text, tidy = TRUE, raw = FALSE) {
  doc  <- xml2::read_html(html_text)
  grid <- .agri_find_data_grid(doc)
  if (is.null(grid)) {
    stop("Could not locate a data table in the response. ",
         "The query may have returned no data.", call. = FALSE)
  }
  if (isTRUE(raw)) return(grid)

  data_rows <- which(grepl(.agri_period_rx, grid[, 1]))
  if (length(data_rows) == 0) {
    # no period column detected; return the grid as a data frame
    df <- as.data.frame(grid, stringsAsFactors = FALSE)
    return(df)
  }
  first_data <- min(data_rows)
  title <- .agri_grid_title(grid, first_data)

  header <- if (first_data > 1) grid[seq_len(first_data - 1), , drop = FALSE] else NULL
  body   <- grid[data_rows, , drop = FALSE]
  nms    <- .agri_build_names(header, ncol(grid))

  # Column 1 is the period; the rest are value columns. Drop all-empty cols.
  period_raw <- body[, 1]
  value_idx  <- setdiff(seq_len(ncol(grid)), 1)
  nonempty   <- vapply(value_idx, function(j) any(nzchar(body[, j])), logical(1))
  value_idx  <- value_idx[nonempty]

  series_names <- nms[value_idx]
  # de-duplicate names
  series_names <- make.unique(series_names, sep = "_")

  year_roc <- suppressWarnings(as.integer(sub(
    "^([0-9]{2,4}).*", "\\1", trimws(period_raw))))
  year_ad  <- ifelse(!is.na(year_roc) & year_roc < 1911, year_roc + 1911, year_roc)

  if (isTRUE(tidy)) {
    out <- do.call(rbind, lapply(seq_along(value_idx), function(k) {
      j <- value_idx[k]
      data.frame(
        period   = period_raw,
        year_roc = year_roc,
        year_ad  = year_ad,
        series   = series_names[k],
        value    = .agri_to_number(body[, j]),
        stringsAsFactors = FALSE
      )
    }))
    rownames(out) <- NULL
  } else {
    out <- data.frame(period = period_raw, year_roc = year_roc,
                      year_ad = year_ad, stringsAsFactors = FALSE)
    for (k in seq_along(value_idx)) {
      out[[series_names[k]]] <- .agri_to_number(body[, value_idx[k]])
    }
  }
  attr(out, "title") <- title
  out
}
