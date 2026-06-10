# Live integration tests. These hit the real MOA website, so they are skipped
# automatically when there is no network access (e.g. on CRAN / CI).

has_net <- function() {
  ok <- tryCatch({
    res <- httr::GET(agrstat:::agri_base_url(), httr::timeout(20))
    !httr::http_error(res)
  }, error = function(e) FALSE)
  ok
}

skip_if_offline <- function() {
  if (!has_net()) skip("No network access to agrstat.moa.gov.tw")
}

test_that("categories can be listed", {
  skip_if_offline()
  cats <- agri_categories()
  expect_s3_class(cats, "data.frame")
  expect_true(all(c("name", "target") %in% names(cats)))
  expect_gt(nrow(cats), 5)
})

test_that("datasets and dimensions resolve", {
  skip_if_offline()
  ds <- agri_datasets("農產品生產面積統計")
  expect_true(nrow(ds) > 0)
  dims <- agri_dimensions("農產品生產面積統計", ds$id[1])
  expect_true(length(dims) >= 1)
  expect_true(all(c("label", "value") %in% names(dims[[1]])))
})

test_that("fetch returns tidy data", {
  skip_if_offline()
  df <- agri_fetch("農產品生產面積統計", "稻米種植面積：縣市別",
                   year_begin = 111, year_end = 113)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("period", "year_roc", "year_ad", "series", "value") %in% names(df)))
  expect_true(is.numeric(df$value))
  expect_true(all(df$year_roc %in% 111:113))
})

test_that("wide format works", {
  skip_if_offline()
  w <- agri_fetch("農產品生產面積統計", "稻米種植面積：縣市別",
                  year_begin = 112, year_end = 113, tidy = FALSE)
  expect_true("period" %in% names(w))
  expect_gt(ncol(w), 3)
})
