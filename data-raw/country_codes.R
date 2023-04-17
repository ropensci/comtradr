## code to prepare `country_codes` dataset goes here

library(tidyverse)

reporter_codes_raw <- jsonlite::fromJSON("https://comtradeapi.un.org/files/v1/app/reference/Reporters.json")$results

reporter_codes <- reporter_codes_raw |>
  transmute(
    id,
    country = text,
    iso_3 = reporterCodeIsoAlpha3,
    entry_year = lubridate::year(entryEffectiveDate),
    exit_year = lubridate::year(entryExpiredDate),
    reporter = T
  )

partner_codes_raw <- jsonlite::fromJSON("https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json")$results

partner_codes <- partner_codes_raw |>
  transmute(
    id,
    country = text,
    iso_3 = PartnerCodeIsoAlpha3,
    entry_year = lubridate::year(entryEffectiveDate),
    exit_year = lubridate::year(entryExpiredDate),
    partner = T
  )

country_codes <- full_join(reporter_codes, partner_codes)

usethis::use_data(country_codes, overwrite = TRUE)
