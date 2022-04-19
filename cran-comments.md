
# R CMD Check

## Test environments
* macOS Catalina 10.15.7, R 4.1.3
* CRAN win-builder, R version 4.2.0 RC (2022-04-15 r82203 ucrt)

## R CMD check results
0 errors | 0 warnings | 1 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Chris Muir <chrismuirRVA@gmail.com>'

------------

I got an email from CRAN indicating the following:

```
'length > 1 in coercion to logical'

'the condition has length > 1' now always gives an error in R pre-4.2.0
and R-devel.

Please correct before 2022-04-26 to safely retain your package on CRAN.
```

This release address this issue, in commit [e9f1437](https://github.com/ropensci/comtradr/commit/e9f14375fddcd077ad5def089553955dcf548549)

------------

Package changes implemented in this version:

* Modifications to `ct_search()` to add support for commodity code `ag6` ([#30](https://github.com/ropensci/comtradr/pull/30))

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
