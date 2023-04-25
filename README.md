
<!-- README.md is generated from README.Rmd. Please edit that file -->
comtradr
========

[![Travis-CI Build Status](https://travis-ci.org/ropensci/comtradr.svg?branch=master)](https://travis-ci.org/ropensci/comtradr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ropensci/comtradr?branch=master&svg=true)](https://ci.appveyor.com/project/ropensci/comtradr) [![codecov](https://codecov.io/github/ropensci/comtradr/branch/master/graphs/badge.svg)](https://codecov.io/github/ropensci/comtradr) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/comtradr)](https://cran.r-project.org/package=comtradr) [![](https://badges.ropensci.org/141_status.svg)](https://github.com/ropensci/software-review/issues/141)
 [![R-CMD-check](https://github.com/ropensci/comtradr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/comtradr/actions/workflows/R-CMD-check.yaml)

**NOTICE** As of early April 2023, this package no longer works as intended. The UN has introduced a new API, this package interacts with the legacy API that no longer is reachable. We are currently looking for someone to take over ownership of this package, see issue [#44](https://github.com/ropensci/comtradr/issues/44).

R package for interacting with the [UN Comtrade Database](https://comtrade.un.org/data/) public API. UN Comtrade provides historical data on the weights and value of specific goods shipped between countries, more info can be found [here](https://comtrade.un.org/). Full API documentation can be found [here](https://comtrade.un.org/data/doc/api/).

This package was inspired by the [R tutorial](https://comtrade.un.org/data/Doc/api/ex/r) posted by Comtrade, and is built using [httr](https://CRAN.R-project.org/package=httr) and [jsonlite](https://CRAN.R-project.org/package=jsonlite).

I've also built a Shiny app for visualizing comtrade shipping data, that's powered by this package. The app can be viewed [here](https://chrismuir.shinyapps.io/comtrade_plot_shinyapp/).

Please [report](https://github.com/ropensci/comtradr/issues) issues, comments, or feature requests.

Please note that this package is released with a [Contributor Code of Conduct](https://ropensci.org/code-of-conduct/). By contributing to this project, you agree to abide by its terms.

For information on citation of this package, use `citation("comtradr")`

Installation
------------

Install from CRAN:

``` r
install.packages("comtradr")
```

Or install from this repo:

``` r
# install.packages("devtools")
devtools::install_github("ropensci/comtradr")
```

Example Usage
-------------

**Example 1**: Return all exports from China to South Korea, United States and Mexico, for all years

``` r
library(comtradr)

# Country names passed to the API query function must be spelled as they appear 
# in the Comtrade DB. Use "ct_country_lookup" to query the country DB and 
#return the exact spelling of specific countries.
ct_country_lookup("korea")
#> [1] "Dem. People's Rep. of Korea" "Rep. of Korea"

# Since we want South Korea, we'll use "Rep. of Korea" within the API query.
example1 <- ct_search(reporters = "China", 
                      partners = c("Rep. of Korea", "USA", "Mexico"), 
                      trade_direction = "exports")

# Inspect the return data
str(example1)
#> 'data.frame':    87 obs. of  35 variables:
#>  $ classification        : chr  "H5" "H5" "H5" "H5" ...
#>  $ year                  : int  2017 2017 2017 2018 2018 2018 2019 2019 2019 2020 ...
#>  $ period                : int  2017 2017 2017 2018 2018 2018 2019 2019 2019 2020 ...
#>  $ period_desc           : chr  "2017" "2017" "2017" "2018" ...
#>  $ aggregate_level       : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ is_leaf_code          : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ trade_flow_code       : int  2 2 2 2 2 2 2 2 2 2 ...
#>  $ trade_flow            : chr  "Export" "Export" "Export" "Export" ...
#>  $ reporter_code         : int  156 156 156 156 156 156 156 156 156 156 ...
#>  $ reporter              : chr  "China" "China" "China" "China" ...
#>  $ reporter_iso          : chr  "CHN" "CHN" "CHN" "CHN" ...
#>  $ partner_code          : int  410 484 842 410 484 842 410 484 842 410 ...
#>  $ partner               : chr  "Rep. of Korea" "Mexico" "USA" "Rep. of Korea" ...
#>  $ partner_iso           : chr  "KOR" "MEX" "USA" "KOR" ...
#>  $ second_partner_code   : logi  NA NA NA NA NA NA ...
#>  $ second_partner        : chr  NA NA NA NA ...
#>  $ second_partner_iso    : chr  NA NA NA NA ...
#>  $ customs_proc_code     : chr  NA NA NA NA ...
#>  $ customs               : chr  NA NA NA NA ...
#>  $ mode_of_transport_code: chr  NA NA NA NA ...
#>  $ mode_of_transport     : chr  NA NA NA NA ...
#>  $ commodity_code        : chr  "TOTAL" "TOTAL" "TOTAL" "TOTAL" ...
#>  $ commodity             : chr  "All Commodities" "All Commodities" "All Commodities" "All Commodities" ...
#>  $ qty_unit_code         : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ qty_unit              : chr  "No Quantity" "No Quantity" "No Quantity" "No Quantity" ...
#>  $ alt_qty_unit_code     : logi  NA NA NA NA NA NA ...
#>  $ alt_qty_unit          : chr  NA NA NA NA ...
#>  $ qty                   : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ alt_qty               : logi  NA NA NA NA NA NA ...
#>  $ netweight_kg          : int  0 0 NA 0 0 0 0 0 0 0 ...
#>  $ gross_weight_kg       : logi  NA NA NA NA NA NA ...
#>  $ trade_value_usd       : num  1.03e+11 3.59e+10 4.30e+11 1.09e+11 4.41e+10 ...
#>  $ cif_trade_value_usd   : logi  NA NA NA NA NA NA ...
#>  $ fob_trade_value_usd   : logi  NA NA NA NA NA NA ...
#>  $ flag                  : int  4 4 4 4 4 4 4 4 4 4 ...
#>  - attr(*, "url")= chr "https://comtrade.un.org/api/get?max=50000&type=C&freq=A&px=HS&ps=all&r=156&p=410%2C842%2C484&rg=2&cc=TOTAL&head=H&fmt=json"
#>  - attr(*, "time_stamp")= POSIXct[1:1], format: "2022-04-17 12:26:21"
#>  - attr(*, "req_duration")= num 3.33
```

**Example 2**: Return all exports related to shrimp from Thailand to all other countries, for years 2007 thru 2011

``` r
library(comtradr)

# Fetch all shrimp related commodity codes from the Comtrade commodities DB. 
# This vector of codes will get passed to the API query.
shrimp_codes <- ct_commodity_lookup("shrimp", return_code = TRUE, return_char = TRUE)

# API query.
example2 <- ct_search(reporters = "Thailand", 
                      partners = "All", 
                      trade_direction = "exports", 
                      start_date = 2007, 
                      end_date = 2011, 
                      commod_codes = shrimp_codes)

# Inspect the output
str(example2)
#> 'data.frame':    1203 obs. of  35 variables:
#>  $ classification        : chr  "H3" "H3" "H3" "H3" ...
#>  $ year                  : int  2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
#>  $ period                : int  2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...
#>  $ period_desc           : chr  "2007" "2007" "2007" "2007" ...
#>  $ aggregate_level       : int  6 6 6 6 6 6 6 6 6 6 ...
#>  $ is_leaf_code          : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ trade_flow_code       : int  2 2 2 2 2 2 2 2 2 2 ...
#>  $ trade_flow            : chr  "Export" "Export" "Export" "Export" ...
#>  $ reporter_code         : int  764 764 764 764 764 764 764 764 764 764 ...
#>  $ reporter              : chr  "Thailand" "Thailand" "Thailand" "Thailand" ...
#>  $ reporter_iso          : chr  "THA" "THA" "THA" "THA" ...
#>  $ partner_code          : int  0 36 40 48 56 104 116 124 152 156 ...
#>  $ partner               : chr  "World" "Australia" "Austria" "Bahrain" ...
#>  $ partner_iso           : chr  "WLD" "AUS" "AUT" "BHR" ...
#>  $ second_partner_code   : logi  NA NA NA NA NA NA ...
#>  $ second_partner        : chr  NA NA NA NA ...
#>  $ second_partner_iso    : chr  NA NA NA NA ...
#>  $ customs_proc_code     : chr  NA NA NA NA ...
#>  $ customs               : chr  NA NA NA NA ...
#>  $ mode_of_transport_code: chr  NA NA NA NA ...
#>  $ mode_of_transport     : chr  NA NA NA NA ...
#>  $ commodity_code        : chr  "030613" "030613" "030613" "030613" ...
#>  $ commodity             : chr  "Shrimps & prawns, whether/not in shell, frozen" "Shrimps & prawns, whether/not in shell, frozen" "Shrimps & prawns, whether/not in shell, frozen" "Shrimps & prawns, whether/not in shell, frozen" ...
#>  $ qty_unit_code         : int  8 8 8 8 8 8 8 8 8 8 ...
#>  $ qty_unit              : chr  "Weight in kilograms" "Weight in kilograms" "Weight in kilograms" "Weight in kilograms" ...
#>  $ alt_qty_unit_code     : logi  NA NA NA NA NA NA ...
#>  $ alt_qty_unit          : chr  NA NA NA NA ...
#>  $ qty                   : int  169654441 5545602 1265 29780 2721318 750 8510 13088545 4930 3410678 ...
#>  $ alt_qty               : logi  NA NA NA NA NA NA ...
#>  $ netweight_kg          : int  169654441 5545602 1265 29780 2721318 750 8510 13088545 4930 3410678 ...
#>  $ gross_weight_kg       : logi  NA NA NA NA NA NA ...
#>  $ trade_value_usd       : int  1084677273 36120291 11888 124668 16061545 4521 74842 77292118 64218 18400152 ...
#>  $ cif_trade_value_usd   : logi  NA NA NA NA NA NA ...
#>  $ fob_trade_value_usd   : logi  NA NA NA NA NA NA ...
#>  $ flag                  : int  0 0 0 0 0 0 0 0 0 0 ...
#>  - attr(*, "url")= chr "https://comtrade.un.org/api/get?max=50000&type=C&freq=A&px=HS&ps=2007%2C2008%2C2009%2C2010%2C2011&r=764&p=all&r"| __truncated__
#>  - attr(*, "time_stamp")= POSIXct[1:1], format: "2022-04-17 12:26:24"
#>  - attr(*, "req_duration")= num 6.95
```

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
