# agrstat <img src="https://yyliou.github.io/hex/agrstat.svg" align="right" height="150" alt="agrstat hex badge" />

`agrstat` provides a programmatic R interface to the *Agricultural
Statistics Dynamic Inquiry* system maintained by the Ministry of
Agriculture (MOA), Taiwan
(<https://agrstat.moa.gov.tw/sdweb/public/inquiry/InquireAdvance.aspx>).

The inquiry system is implemented as an ASP.NET WebForms application in
which every interaction is a stateful, full-page POST carrying the
`__VIEWSTATE` and `__EVENTVALIDATION` tokens. `agrstat` reproduces this
interaction sequence faithfully and exposes it as a small set of
composable functions, so that the choice of statistical category,
dataset, classification strata, and time span is controlled entirely
through function arguments. Query results are returned as tidy data
frames suitable for downstream analysis.

## Installation

```r
# Runtime dependencies
install.packages(c("httr", "xml2", "rvest"))

# Install from source
# install.packages("remotes")
remotes::install_local("path/to/agri")

# Or, during development
# devtools::load_all("path/to/agri")
```

## Design

The inquiry workflow is decomposed into four operations, three of which
are exploratory and one of which performs retrieval:

| Stage | Function | Purpose |
|-------|----------|---------|
| Enumerate categories | `agri_categories()` | List the statistical categories on the landing page |
| Enumerate datasets | `agri_datasets(category)` | List the datasets (statistical tables) within a category |
| Inspect strata | `agri_dimensions(category, dataset)` | List the members of each classification axis |
| Retrieve data | `agri_fetch(category, dataset, ...)` | Execute the query and return a data frame |

All functions accept an optional `agri_session` object, allowing the
underlying HTTP session (and thus the ASP.NET session cookie) to be
reused across calls.

## Exploration

```r
library(agrstat)

## Statistical categories
cats <- agri_categories()
head(cats)
#>             name   target
#> 1   地理及環境統計  ...ctl08
#> 2       土地統計   ...ctl09

## Datasets within a category (matched by name or target)
ds <- agri_datasets("農產品生產面積統計")
ds
#>   id                               name
#> 1 79               稻米種植面積：縣市別
#> 2 80   稻米收穫面積：縣市別×期作別×稻種別

## Classification axes and their members
dims <- agri_dimensions("農產品生產面積統計",
                        "稻米收穫面積：縣市別×期作別×稻種別")
names(dims)        # axis1, axis2, axis3
lapply(dims, head) # label / value for each axis
```

## Retrieval

```r
## Default: all strata members across all axes and the full
## available time span, returned in long (tidy) form.
df <- agri_fetch("農產品生產面積統計", "稻米種植面積：縣市別")
head(df)
#>   period year_roc year_ad series  value
#> 1  104年      104    2015   合計 251888
#> 2  105年      105    2016   合計 273866

## Restrict the time span (years are given in the ROC calendar)
df <- agri_fetch("農產品生產面積統計", "稻米種植面積：縣市別",
                 year_begin = 110, year_end = 113)

## Restrict the strata. The `dimensions` argument takes one element per
## axis, either positional or named (axis1, axis2, ...); omitted axes
## default to all members.
df <- agri_fetch(
  "農產品生產面積統計",
  "稻米收穫面積：縣市別×期作別×稻種別",
  dimensions = list(
    axis1 = c("合計", "臺中市", "雲林縣"),  # county
    axis2 = "一期",                          # crop season
    axis3 = "all"                            # rice variety
  ),
  year_begin = 111, year_end = 113
)

## Wide form: one column per series
wide <- agri_fetch("農產品生產面積統計", "稻米種植面積：縣市別",
                   tidy = FALSE)

## Raw, untidied grid (for inspection of irregular tables)
raw <- agri_fetch("農產品生產面積統計", "稻米種植面積：縣市別",
                  raw = TRUE)
```

### Exporting to CSV

```r
df <- agri_fetch("農產品生產面積統計", "稻米種植面積：縣市別")
write.csv(df, "rice_planting_area.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
```

## Arguments of `agri_fetch()`

- `category` — category name or target (see `agri_categories()`);
  partial string matching is supported.
- `dataset` — dataset id or name (see `agri_datasets()`).
- `dimensions` — `NULL` selects every member of every axis; otherwise a
  list with one element per axis (positional, or named `axis1`,
  `axis2`, …), each a character vector of member labels (or values), or
  the string `"all"`.
- `year_begin`, `year_end` — start and end of the time span in ROC years
  (e.g. `110`); `NULL` selects the full available range.
- `tidy` — if `TRUE` (default) the result is returned in long form with
  columns `period`, `year_roc`, `year_ad`, `series`, and `value`; if
  `FALSE`, a wide data frame is returned.
- `raw` — if `TRUE`, the untidied character grid is returned.
- `ses` — an optional existing `agri_session`.

The returned data frame carries the metadata attributes `title`
(including units), `category`, `dataset`, and `fetched_at`.

## Implementation

`agrstat` mirrors the browser interaction exactly:

1. issue a `GET` request to the inquiry page and parse the form,
   retaining the hidden `__VIEWSTATE` / `__EVENTVALIDATION` state;
2. post back into a category (`__EVENTTARGET = ctl…`) to obtain the
   dataset list;
3. switch the `lstFieldGroup` control (auto-postback) to load the
   classification axes of the selected dataset;
4. select members in each `lstDimension` list box and submit
   *查詢確認* (`btnQuery`);
5. set `ddlYearBegin` / `ddlYearEnd` and re-submit *查詢*
   (`btnQuery2`);
6. parse the resulting HTML table with `rvest::html_table(fill = TRUE)`,
   which resolves the multi-level `colspan` / `rowspan` header, then
   separate the header rows from the data rows, construct column names,
   and coerce the cells to numeric values.

## Notes and limitations

- The default behaviour (all strata across all axes, over the full time
  span) retrieves the entire contents of a dataset. For
  multi-axis datasets the number of value columns equals the product of
  the selected members per axis and may therefore be large; restrict the
  query through `dimensions` where appropriate.
- A small number of categories are exposed on the landing page as direct
  links (`CategoryLink.ashx`) rather than postbacks; the present
  implementation targets the postback categories.
- The package depends on the current page structure of the official
  website; substantial redesigns of that site may require updates to the
  parsing logic.

## License

MIT
