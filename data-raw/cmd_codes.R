## code to prepare `cmd_codes` dataset goes here

library(tidyverse)

hs_cmd_codes_raw <- jsonlite::fromJSON("https://comtrade.un.org/Data/cache/classificationHS.json")$results

hs_cmd_codes <- hs_cmd_codes_raw |>
  mutate(
    text = str_remove(text, "\\w{1,6} - "),
    parent = na_if(parent, "#")
  )

cmd_codes <- hs_cmd_codes

usethis::use_data(cmd_codes, overwrite = TRUE)
