
<!-- README.md is generated from README.Rmd. Please edit that file -->

# comtradr

<!-- badges: start -->
<!-- [![Travis-CI Build Status](https://travis-ci.org/ropensci/comtradr.svg?branch=master)](https://travis-ci.org/ropensci/comtradr) -->
<!-- [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ropensci/comtradr?branch=master&svg=true)](https://ci.appveyor.com/project/ropensci/comtradr) -->

[![codecov](https://codecov.io/github/ropensci/comtradr/branch/master/graphs/badge.svg)](https://codecov.io/github/ropensci/comtradr)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/comtradr)](https://cran.r-project.org/package=comtradr)
[![](https://badges.ropensci.org/141_status.svg)](https://github.com/ropensci/software-review/issues/141)
[![R-CMD-check](https://github.com/ropensci/comtradr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/comtradr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<!-- R package for interacting with the [UN Comtrade Database](https://comtrade.un.org/data/) public API. UN Comtrade provides historical data on the weights and value of  -->
<!-- specific goods shipped between countries, more info can be found [here](https://comtrade.un.org/). Full API documentation can be found  -->
<!-- [here](https://comtrade.un.org/data/doc/api/). -->

## Currently re-launching

See \#44 for the discussion around it.

Please [report](https://github.com/ropensci/comtradr/issues) issues,
comments, or feature requests.

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

For information on citation of this package, use `citation("comtradr")`

## Installation

<!-- Install from CRAN: -->
<!-- ```{r eval=FALSE} -->
<!-- install.packages("comtradr") -->
<!-- ``` -->

Install the current development version from this repo:

``` r
# install.packages("devtools")
devtools::install_github("ropensci/comtradr@api-update")
```

<!-- ## Example Usage -->
<!-- **Example 1**: Return all exports from China to South Korea, United States and Mexico, for all years -->
<!-- ```{r, eval = F} -->
<!-- library(comtradr) -->
<!-- # Country names passed to the API query function must be spelled as they appear  -->
<!-- # in the Comtrade DB. Use "ct_country_lookup" to query the country DB and  -->
<!-- #return the exact spelling of specific countries. -->
<!-- ct_country_lookup("korea") -->
<!-- # Since we want South Korea, we'll use "Rep. of Korea" within the API query. -->
<!-- example1 <- ct_search(reporters = "China",  -->
<!--                       partners = c("Rep. of Korea", "USA", "Mexico"),  -->
<!--                       trade_direction = "exports") -->
<!-- # Inspect the return data -->
<!-- str(example1) -->
<!-- ``` -->
<!-- **Example 2**: Return all exports related to shrimp from Thailand to all other countries, for years 2007 thru 2011 -->
<!-- ```{r, eval = F} -->
<!-- library(comtradr) -->
<!-- # Fetch all shrimp related commodity codes from the Comtrade commodities DB.  -->
<!-- # This vector of codes will get passed to the API query. -->
<!-- shrimp_codes <- ct_commodity_lookup("shrimp", return_code = TRUE, return_char = TRUE) -->
<!-- # API query. -->
<!-- example2 <- ct_search(reporters = "Thailand",  -->
<!--                       partners = "All",  -->
<!--                       trade_direction = "exports",  -->
<!--                       start_date = 2007,  -->
<!--                       end_date = 2011,  -->
<!--                       commod_codes = shrimp_codes) -->
<!-- # Inspect the output -->
<!-- str(example2) -->
<!-- ``` -->

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
