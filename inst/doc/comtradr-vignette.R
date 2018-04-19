## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE, fig.width = 9, fig.height = 6)

## ----eval = FALSE--------------------------------------------------------
#  install.packages("comtradr")

## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("ChrisMuir/comtradr")

## ------------------------------------------------------------------------
library(comtradr)

## ------------------------------------------------------------------------
q <- ct_search(reporters = "USA", 
               partners = c("Germany", "France", "Japan", "Mexico"), 
               trade_direction = "imports")

# API calls return a tidy data frame.
str(q)

## ---- eval = FALSE-------------------------------------------------------
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Japan", "Mexico"),
#                 trade_direction = "imports",
#                 start_date = 2010,
#                 end_date = 2014)

## ---- eval = FALSE-------------------------------------------------------
#  # Get all monthly data for a single year (API max of 12 months per call).
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Japan", "Mexico"),
#                 trade_direction = "imports",
#                 start_date = 2012,
#                 end_date = 2012,
#                 freq = "monthly")
#  
#  # Get monthly data for a specific span of months (API max of five months per call).
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Japan", "Mexico"),
#                 trade_direction = "imports",
#                 start_date = "2012-03",
#                 end_date = "2012-07",
#                 freq = "monthly")

## ------------------------------------------------------------------------
ct_country_lookup("korea", "reporter")
ct_country_lookup("bolivia", "partner")

## ---- eval = FALSE-------------------------------------------------------
#  q <- ct_search(reporters = "Rep. of Korea",
#                 partners = "Bolivia (Plurinational State of)",
#                 trade_direction = "all")

## ------------------------------------------------------------------------
ct_commodity_lookup("tomato")

## ---- eval = FALSE-------------------------------------------------------
#  tomato_codes <- ct_commodity_lookup("tomato",
#                                      return_code = TRUE,
#                                      return_char = TRUE)
#  
#  q <- ct_search(reporters = "USA",
#                 partners = c("Germany", "France", "Mexico"),
#                 trade_direction = "all",
#                 commod_codes = tomato_codes)

## ------------------------------------------------------------------------
q <- ct_search(reporters = "USA", 
               partners = c("Germany", "France", "Mexico"), 
               trade_direction = "all", 
               commod_codes = c("0702", "070200", "2002", "200210", "200290"))

## ------------------------------------------------------------------------
# The url of the API call.
attributes(q)$url
# The date-time of the API call.
attributes(q)$time_stamp

# The total duration of the API call, in seconds.
attributes(q)$req_duration

## ------------------------------------------------------------------------
ct_country_lookup(c("Belgium", "vietnam", "brazil"), "reporter")

ct_commodity_lookup(c("tomato", "trout"), return_char = TRUE)

## ------------------------------------------------------------------------
ct_commodity_lookup(c("tomato", "trout"), return_char = FALSE)

## ------------------------------------------------------------------------
ct_commodity_lookup(c("tomato", "sldfkjkfdsklsd"), verbose = TRUE)

## ------------------------------------------------------------------------
ct_update_databases()

## ------------------------------------------------------------------------
ct_commodity_db_type()

## ------------------------------------------------------------------------
# Column headers returned from function ct_search
colnames(q)

## ------------------------------------------------------------------------
# Apply polished column headers
q <- ct_use_pretty_cols(q)

# Print new column headers.
colnames(q)

