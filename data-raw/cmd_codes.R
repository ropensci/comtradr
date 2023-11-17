## code to prepare `cmd_codes` dataset goes here

library(tidyverse)

# Get HS codes ------------------------------------------------------------

hs_cmd_codes_raw <- jsonlite::fromJSON("https://comtradeapi.un.org/files/v1/app/reference/HS.json")$results # nolint

hs_cmd_codes <- hs_cmd_codes_raw |>
  transmute(
    id,
    text = str_remove(text, "\\w{1,6} - "),
    parent = na_if(parent, "-1"),
    aggr_level = aggrLevel,
    class_code = "HS"
  )

# Compile all codes into one dataset --------------------------------------

cmd_codes <- hs_cmd_codes

usethis::use_data(cmd_codes, overwrite = TRUE)
