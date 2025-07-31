## code to prepare `country_codes` dataset goes here

library(tidyverse)

# Get reporter codes ------------------------------------------------------

reporter_codes_raw <- jsonlite::fromJSON("https://comtradeapi.un.org/files/v1/app/reference/Reporters.json")$results # nolint

reporter_codes <- reporter_codes_raw |>
  transmute(
    id,
    country = text,
    iso_3 = reporterCodeIsoAlpha3,
    iso_2 = reporterCodeIsoAlpha2,
    note = reporterNote,
    entry_year = lubridate::year(entryEffectiveDate),
    exit_year = lubridate::year(entryExpiredDate),
    group = isGroup,
    reporter = T
  )

# Get partner codes -------------------------------------------------------

partner_codes_raw <- jsonlite::fromJSON("https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json")$results # nolint

partner_codes <- partner_codes_raw |>
  transmute(
    id,
    country = text,
    iso_3 = if_else(isGroup == TRUE, text, PartnerCodeIsoAlpha3),
    entry_year = lubridate::year(entryEffectiveDate),
    exit_year = lubridate::year(entryExpiredDate),
    group = isGroup,
    partner = T
  )

# Consolidate datasets ----------------------------------------------------

country_codes <- full_join(reporter_codes, partner_codes)

reporter_codes <- country_codes |>
  filter(reporter == T) |>
  select(iso_3, id, group)

partner_codes <- country_codes |>
  filter(partner == T) |>
  select(iso_3, id, group)

# Save external datasets --------------------------------------------------

# usethis::use_data(country_codes, overwrite = TRUE)

# Save internal datasets --------------------------------------------------
#
# usethis::use_data(reporter_codes,
#                   partner_codes,
#                   internal = TRUE, overwrite = TRUE)
