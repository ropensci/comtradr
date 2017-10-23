## Test environments
* local Windows 10 install, R 3.4.2
* local OS X install, R 3.4.2
* ubuntu 14.04.5 (on travis-ci), R 3.4.2
* CRAN win-builder, R Under development (unstable) (2017-09-12 r73242)

## R CMD check results
0 errors | 0 warnings | 1 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Chris Muir <chrismuirRVA@gmail.com>'

## Downstream dependencies
None

---

Package changes implemented in this version:

### PKG API CHANGES

* Eliminated functions `ct_commodities_table` and `ct_countries_table`.
* Added new functions `ct_update_databases`, `ct_use_pretty_cols`, `ct_commodity_db_type`, `ct_register_token`, `ct_get_reset_time`, `ct_get_remaining_hourly_queries`.
* Renamed functions: `commodity_lookup` is now `ct_commodity_lookup`, `country_lookup` is now `ct_country_lookup`.
* The commodity and country reference tables are now saved as cached package data, and accessed by `comtradr` functions when necessary. This replaces the need for functions `ct_commodities_table` and `ct_countries_table`.
* Reorder function arguments within function `ct_search`.
* Changed some function argument names to ensure `snake_case` is being used throughout the package.
* `ct_search` now returns a data frame, as opposed to a list.

### MINOR CHANGES

* Added a vignette directory, with an "Intro to comtradr" vignette.
* API requests are now throttled based on the [rate limits](https://comtrade.un.org/data/doc/api/#Limits) imposed by the UN Comtrade.
* Added function for setting a valid API key/token (`ct_register_token`).
* Appending API metadata to each returned data frame as attributes (url of the API call, date-time of the query, duration of the query in seconds).
* Added package level man page.
* Now using native R errors/warnings, as opposed to nesting API status codes in a returned list.
* `Imports` changes: remove `dplyr`, add `magrittr` and `purrr`.
* Expand and improve test coverage via [testthat](https://github.com/hadley/testthat).

### BUG FIXES

* The issues related to type-safety in function `commodity_lookup` have been fixed by importing `purrr` and using `purrr::map` in place of `sapply`. This fixes [issue #2](https://github.com/ropensci/comtradr/issues/2) and [issue #3](https://github.com/ropensci/comtradr/issues/3).
