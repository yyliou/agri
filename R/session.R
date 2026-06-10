# ---------------------------------------------------------------------------
# Session handling and the generic ASP.NET WebForms "form replay" engine.
#
# The target site is a classic ASP.NET WebForms application. Every interaction
# is a full-page POST that echoes back the entire form, including the
# __VIEWSTATE / __EVENTVALIDATION hidden fields that must be carried forward
# unchanged into the next request. We therefore:
#
#   1. keep a single curl handle so the ASP.NET session cookie persists;
#   2. after every request, parse the returned HTML form into a list of
#      field name -> value pairs (the "page state");
#   3. to perform an action we copy that state, override a few fields
#      (__EVENTTARGET, a button name, some <select> values) and POST it back.
# ---------------------------------------------------------------------------

agri_base_url <- function() {
  "https://agrstat.moa.gov.tw/sdweb/public/inquiry/InquireAdvance.aspx"
}

# Prefix shared by every server control on the inquiry user-control.
agri_ctl_prefix <- function() "ctl00$cphMain$uctlInquireAdvance$"

#' Create a query session for the MOA agricultural statistics site
#'
#' Opens a connection to the dynamic-inquiry page, retrieves the initial
#' page state (view-state plus the list of statistic categories) and returns
#' a session object that the other functions operate on.
#'
#' @param verbose Logical; if TRUE, progress messages are printed.
#' @param timeout Request timeout in seconds (default 60).
#' @return An object of class \code{agri_session}.
#' @export
agri_session <- function(verbose = FALSE, timeout = 60) {
  h <- httr::handle(agri_base_url())
  ses <- structure(
    list(
      handle  = h,
      url     = agri_base_url(),
      timeout = timeout,
      verbose = isTRUE(verbose),
      html    = NULL,   # raw HTML of the current page
      state   = NULL,   # named list: form field -> value
      stage   = "init"
    ),
    class = "agri_session"
  )
  res <- httr::GET(
    ses$url,
    httr::user_agent(agri_user_agent()),
    httr::timeout(timeout),
    handle = h
  )
  httr::stop_for_status(res)
  ses$html  <- httr::content(res, as = "text", encoding = "UTF-8")
  ses$state <- .agri_parse_state(ses$html)
  ses$stage <- "landing"
  if (ses$verbose) message("Session opened; landing page loaded.")
  ses
}

agri_user_agent <- function() {
  "Mozilla/5.0 (compatible; agrstat R package; +https://agrstat.moa.gov.tw)"
}

#' @export
print.agri_session <- function(x, ...) {
  cat("<agri_session>\n")
  cat("  stage :", x$stage, "\n")
  cat("  url   :", x$url, "\n")
  cat("  fields:", length(x$state), "form fields in current page\n")
  invisible(x)
}

# --- HTML form -> named list of fields -------------------------------------

# Parse a returned page into the set of fields that the browser would submit:
# every non-button <input> (hidden or text) plus, for each <select>, the
# value of the selected option (or the first option when none is selected).
.agri_parse_state <- function(html_text) {
  doc <- xml2::read_html(html_text)
  state <- list()

  inputs <- xml2::xml_find_all(doc, "//input[@name]")
  for (nd in inputs) {
    name <- xml2::xml_attr(nd, "name")
    type <- tolower(xml2::xml_attr(nd, "type"))
    if (is.na(type) || type == "") type <- "text"
    if (type %in% c("submit", "image", "button", "reset", "radio")) next
    if (type == "checkbox") {
      checked <- xml2::xml_attr(nd, "checked")
      if (!is.na(checked)) {
        v <- xml2::xml_attr(nd, "value")
        state[[name]] <- if (is.na(v) || v == "") "on" else v
      }
      next
    }
    v <- xml2::xml_attr(nd, "value")
    state[[name]] <- if (is.na(v)) "" else v
  }

  selects <- xml2::xml_find_all(doc, "//select[@name]")
  for (nd in selects) {
    name <- xml2::xml_attr(nd, "name")
    opts <- xml2::xml_find_all(nd, ".//option")
    if (length(opts) == 0) next
    vals <- vapply(opts, function(o) {
      v <- xml2::xml_attr(o, "value")
      if (is.na(v)) xml2::xml_text(o) else v
    }, character(1))
    sel <- vapply(opts, function(o) !is.na(xml2::xml_attr(o, "selected")),
                  logical(1))
    chosen <- if (any(sel)) vals[sel][1] else vals[1]
    state[[name]] <- chosen
  }
  state
}

# --- POST a (possibly multi-valued) field set ------------------------------

# Build an application/x-www-form-urlencoded body. Values may be character
# vectors of length > 1 (multi-select list boxes) in which case the key is
# repeated, exactly as a browser submits multiple selected <option>s.
.agri_encode_body <- function(params) {
  parts <- character(0)
  for (key in names(params)) {
    val <- params[[key]]
    if (length(val) == 0) val <- ""
    ekey <- utils::URLencode(key, reserved = TRUE)
    for (v in as.character(val)) {
      parts <- c(parts, paste0(ekey, "=", utils::URLencode(v, reserved = TRUE)))
    }
  }
  paste(parts, collapse = "&")
}

# Core action: take the current page state, apply overrides, POST, and store
# the new page state on the session. `overrides` is a named list; a value of
# NULL removes that key. `event_target` / `button` are convenience args.
.agri_submit <- function(ses, overrides = list(), event_target = "",
                         button = NULL) {
  params <- ses$state
  params[["__EVENTTARGET"]]   <- event_target
  params[["__EVENTARGUMENT"]] <- ""
  if (!is.null(button)) params[[button$name]] <- button$value
  for (k in names(overrides)) {
    if (is.null(overrides[[k]])) params[[k]] <- NULL
    else params[[k]] <- overrides[[k]]
  }

  body <- .agri_encode_body(params)
  res <- httr::POST(
    ses$url,
    body = body,
    httr::content_type("application/x-www-form-urlencoded"),
    httr::user_agent(agri_user_agent()),
    httr::timeout(ses$timeout),
    handle = ses$handle
  )
  httr::stop_for_status(res)
  ses$html  <- httr::content(res, as = "text", encoding = "UTF-8")
  ses$state <- .agri_parse_state(ses$html)
  ses
}

# Return the current page's parsed xml document.
.agri_doc <- function(ses) xml2::read_html(ses$html)
