# comtradr 1.0.4

* patched ct_get_ref_table to also return reporterCodeIsoAlpha2 and partnerCodeIsoAlpha2, see: https://github.com/ropensci/comtradr/issues/94
* patched an error, where the wrong object was initialised for EB02 for querying services, see: https://github.com/ropensci/comtradr/issues/96 
* thanks to luifrancgom and possakorn for pointing and fixing errors


# comtradr 1.0.3

* this is a patch to fix a bug that was introduced when fixing the bug about tidy cols. Now tidy cols are returned again. 
* testing for this error was introduced in the test environment to avoid this in the future.

# comtradr 1.0.2

* this is a patch to fix two bugs in the package: 
    * a missing return in the call to tidy_cols did not allow users to set this argument to false, see https://github.com/ropensci/comtradr/issues/88 and commit 6f34592 (thanks @FATelarico)
    * a missing parameter in the file size conversion function did not allow the bulk facility to download small files. This has been fixed. 

# comtradr 1.0.1

* this is a patch to fix a CRAN policy violation, namely that the cache was written in the wrong directory due to using rappdirs.

* If you have used comtradr prior to version 1.0.1 I recommend running `ct_migrate_cache()` to copy files to the new cache and remove the old cache

# comtradr 1.0.0

* with this update comtradr has reached full functionality, including bulk files

## New features

* **BULK DOWNLOAD ADDED:** the function `ct_get_bulk` has been added. 
It enables the use of the bulk download facility from Comtrade. 

* Please see the vignette 
https://docs.ropensci.org/comtradr/articles/bulk_files.html for details

## Bug fixes

* The check for the parameter flowCode did not allow the value "everything"
as it should. Now it does (commit 279cf03)






# comtradr 0.4.0.0


* comtradr has received a complete overhaul. Please reference the Readme for all new functions and use-cases.

## NEW FEATURES

* **Caching Functionality**: Implemented enhanced caching features, and added a dedicated vignette explaining caching concepts. Environment variables allow for configuration of some caching features.

* **Functionality Updates**: Major updates to key functions such as `ct_get_data` and `ct_get_ref_table`, including parameter adjustments and usability improvements, mostly checks for all important parameters before the request is send out.

### Deprecated in Version 0.4.0

* **ct_commodity_db_type**
  * Deprecated with no direct alternative.

* **ct_country_lookup**
  * Deprecated, use `country_codes` dataset for country codes.

* **ct_search**
  * Deprecated, use `ct_get_data()` as an alternative.

* **ct_update_databases**
  * Deprecated, utilize the `update` parameter in `ct_get_data`.

* **ct_use_pretty_cols**
  * Deprecated, replaced by the `process` argument in main functions.

* **ct_get_reset_time**
  * Deprecated, no longer relevant due to daily call limit enforcement.

* **ct_get_remaining_hourly_queries**
  * Deprecated, as hourly reset time no longer exists.

* **ct_register_token**
  * Deprecated, use `set_primary_comtrade_key()` instead.
  
  
# comtradr 0.3.0.09000

* No news yet!


# comtradr 0.3.0

## NEW FEATURES

* Modifications to `ct_search()` to add support for commodity code `ag6` ([#30](https://github.com/ropensci/comtradr/pull/30))

## BUG FIXES

* Function `ct_register_token()` now checks if the provided token is recognized by the official API and only grants "premium" credentials if it is ([#34](https://github.com/ropensci/comtradr/issues/34)).

* Passing an API token string to `ct_register_token()` now properly bumps the hourly rate limit up to 10,000
([#21](https://github.com/ropensci/comtradr/issues/21)).

* In func `ct_search()`, passing a character vector of long-form commodity descriptions to arg `commod_codes` will now 
throw an error prior to making an API call, which would fail ([#24](https://github.com/ropensci/comtradr/issues/24)).

* Update the country package data, to stay up to date with the reporter/partner country table that Comtrade is using. This is an update to
the file `inst/extdata/country_table.rda`. ([#29](https://github.com/ropensci/comtradr/issues/29)).

* In func `ct_search()`, improve error messaging when an input country is invalid. ([#31](https://github.com/ropensci/comtradr/issues/31)).

* In func `ct_search()`, fix bug in which running queries using the `SITCrev2` commodity type was returning raw HTML (as opposed to json data). ([#27](https://github.com/ropensci/comtradr/issues/27)).

* In func `ct_country_lookup()`, remove `NA` inputs from multi-country lookups (previously `NA` was being stringified and included in the pipe-separated regex lookup).


# comtradr 0.2.2


* Remove unused dependency `methods` from `Imports`.

# comtradr 0.2.1

## NEW FEATURES

* Modifications to `ct_search()` to allow for pulling all monthly data for an entire year in a single query ([#14](https://github.com/ropensci/comtradr/issues/14))
* For function `ct_search()`, expanded the valid input types for args `start_date` and `end_date` ([#10](https://github.com/ropensci/comtradr/issues/10)).

## BUG FIXES

* `ct_search()` now supports all commodity classifications offered by UN Comtrade ([#16](https://github.com/ropensci/comtradr/issues/16)).
* The updates generated by function `ct_update_databases()` are now properly preserved between R sessions ([#11](https://github.com/ropensci/comtradr/issues/11)).
* Passing `"services"` to arg `type` within function `ct_search()` now uses commodity classification `EB02` by default (previously this would throw an error, fixes [#6](https://github.com/ropensci/comtradr/issues/6)).
* When using commodity classification `EB02` within function `ct_search()`, passing `"TOTAL"` to arg `commod_codes` no longer returns zero results ([#7](https://github.com/ropensci/comtradr/issues/7)).
* `ct_commodity_lookup()` no longer returns zero results when passing all caps input to arg `search_terms` ([#9](https://github.com/ropensci/comtradr/issues/9)).


# comtradr 0.1.0


## PKG API CHANGES

* Eliminated functions `ct_commodities_table` and `ct_countries_table`.
* Added new functions `ct_update_databases`, `ct_use_pretty_cols`, `ct_commodity_db_type`, `ct_register_token`, `ct_get_reset_time`, `ct_get_remaining_hourly_queries`.
* Renamed functions: `commodity_lookup` is now `ct_commodity_lookup`, `country_lookup` is now `ct_country_lookup`.
* The commodity and country reference tables are now saved as cached package data, and accessed by `comtradr` functions when necessary. This replaces the need for functions `ct_commodities_table` and `ct_countries_table`.
* Reorder function arguments within function `ct_search`.
* Changed some function argument names to ensure `snake_case` is being used throughout the package.
* `ct_search` now returns a data frame, as opposed to a list.

## MINOR CHANGES

* Added a vignette directory, with an "Intro to comtradr" vignette.
* API requests are now throttled based on the [rate limits](https://uncomtrade.org/docs/subscriptions/) imposed by the UN Comtrade.
* Added function for setting a valid API key/token (`ct_register_token`).
* Appending API metadata to each returned data frame as attributes (url of the API call, date-time of the query, duration of the query in seconds).
* Added package level man page.
* Now using native R errors/warnings, as opposed to nesting API status codes in a returned list.
* `Imports` changes: remove `dplyr`, add `magrittr` and `purrr`.
* Expand and improve test coverage via [testthat](https://github.com/r-lib/testthat).

## BUG FIXES

* The issues related to type-safety in function `commodity_lookup` have been fixed by importing `purrr` and using `purrr::map` in place of `sapply`. This fixes [issue #2](https://github.com/ropensci/comtradr/issues/2) and [issue #3](https://github.com/ropensci/comtradr/issues/3).


# comtradr 0.0.2 (2017-07-03)

## NEW FEATURES

* commodity_lookup(): Expanded function to accept multiple commodities or commodity codes (as either character vector or numeric vector). Also added argument "return_char" that allows the user to specify list output or char vector output, and argument "return_code" that specifies output of commodity descriptions or commodity codes.

## MINOR IMPROVEMENTS

* Add unit tests via [testthat](https://github.com/r-lib/testthat).


# comtradr 0.0.1 (2017-04-06)

## NEW FEATURES

* released to CRAN
