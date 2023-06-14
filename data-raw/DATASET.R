library(httr2)
library(poorman)
library(stringr)
library(readr)
library(lubridate)
library(usethis)

# getting comtrade data ---------------------------------------------------

## getting list of reference tables
response <- httr2::request('https://comtradeapi.un.org/files/v1/app/reference/ListofReferences.json') |>
  httr2::req_perform()

## getting data from response of list of tables
list_of_datasets <- response |>
  httr2::resp_body_json(simplifyVector = T) |>
  purrr::pluck(1)

## getting date of last modification from list of tables
last_modified <- httr2::resp_header(header = "Last-Modified", resp = response) |>
  stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
  as.Date(format = "%d %b %Y")

## writing last modification to data
list_of_datasets$last_modified <- last_modified

## changing colon to underscore in dataset names
list_of_datasets$category <- stringr::str_replace_all(list_of_datasets$category,':',"_") |>
  tolower()

## save list of datasets
save(list_of_datasets, file = 'inst/extdata/list_of_datasets.rda')

i <- 1
## loop over all datasets (for loop, because it is readable,
## no need for speeding this up with more complicated lapply or more dependencies)
for(i in seq_along(list_of_datasets$category)){
  ## define the valid commodity codes that we need
  valid_cmd_datasets <- c('cmd_hs', 'cmd_s1', 'cmd_s2', 'cmd_s3', 'cmd_s4',
                      'cmd_ss', 'cmd_b4', 'cmd_b5', 'cmd_eb02', 'cmd_eb10',
                      'cmd_eb10s', 'cmd_eb')

  valid_country_datasets <- c('reporter','partner')
  valid_other_datasets <- c('mot','customs')

  ## if it is a valid dataset that we need, download it
  if(list_of_datasets$category[i] %in% valid_cmd_datasets){
    response <- httr2::request(list_of_datasets$fileuri[i]) |>
      httr2::req_perform()

    data <- response |>
      httr2::resp_body_json(simplifyVector = T)

    last_modified <- httr2::resp_header(header = "Last-Modified", resp = response) |>
      stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
      as.Date(format = "%d %b %Y")

    result <- data$results

    result$last_modified <- last_modified

    readr::write_rds(result, "xz",
                     file = paste0('inst/extdata/',list_of_datasets$category[i],'.rds'))
  } else if(list_of_datasets$category[i] %in% valid_country_datasets) {
    response <- httr2::request(list_of_datasets$fileuri[i]) |>
      httr2::req_perform()

    data <- response |>
      httr2::resp_body_json(simplifyVector = T)

    last_modified <- httr2::resp_header(header = "Last-Modified", resp = response) |>
      stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
      as.Date(format = "%d %b %Y")

    if(list_of_datasets$category[i]=='reporter'){
      result <- data$results |>
        poorman::transmute(
          id,
          country = text,
          iso_3 = reporterCodeIsoAlpha3,
          entry_year = lubridate::year(entryEffectiveDate),
          exit_year = lubridate::year(entryExpiredDate),
          group = isGroup
        )
    } else {
      result <- data$results |>
        poorman::transmute(
          id,
          country = text,
          iso_3 = PartnerCodeIsoAlpha3,
          entry_year = lubridate::year(entryEffectiveDate),
          exit_year = lubridate::year(entryExpiredDate),
          group = isGroup
        ) |>
        poorman::mutate(iso_3 = ifelse(country=='World','World',iso_3))

    }

    result$last_modified <- last_modified

    readr::write_rds(result, "xz",
                     file = paste0('inst/extdata/',list_of_datasets$category[i],'.rds'))
  } else if(list_of_datasets$category[i] %in% valid_other_datasets) {
    response <- httr2::request(list_of_datasets$fileuri[i]) |>
      httr2::req_perform()

    data <- response |>
      httr2::resp_body_json(simplifyVector = T)

    last_modified <- httr2::resp_header(header = "Last-Modified", resp = response) |>
      stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
      as.Date(format = "%d %b %Y")

    result <- data$results

    result$last_modified <- last_modified

    readr::write_rds(result, "xz",
                     file = paste0('inst/extdata/',list_of_datasets$category[i],'.rds'))
  } else {
    next
  }
}

# Consolidate datasets ----------------------------------------------------
reporter_codes <- readr::read_rds(paste0('inst/extdata/','reporter','.rds')) |>
  poorman::mutate(reporter =T)
partner_codes <- readr::read_rds(paste0('inst/extdata/','partner','.rds'))|>
  poorman::mutate(partner =T)
country_codes <- poorman::full_join(reporter_codes, partner_codes)

# Save external datasets --------------------------------------------------

usethis::use_data(country_codes, overwrite = TRUE)





# Data for vignette -------------------------------------------------------

example_1 <- comtradr::ct_get_data(
  reporter = 'USA',
  partner = c('DEU', 'FRA','JPN','MEX'),
  commodity_code = 'TOTAL',
  start_date = 2018,
  end_date = 2023,
  flow_direction = 'import'
)
save(example_1, file = 'inst/extdata/vignette_data_1.rda')

example_2 <- comtradr::ct_get_data(
  reporter = 'CHN',
  partner = c('KOR', 'USA','MEX'),
  commodity_code = 'TOTAL',
  start_date = 2012,
  end_date = 2023,
  flow_direction = 'export'
)
save(example_2, file = 'inst/extdata/vignette_data_2.rda')


shrimp_codes <- ct_commodity_lookup("shrimp",
                                    return_code = TRUE,
                                    return_char = TRUE)

# Comtrade api query.
example_3 <- ct_get_data(reporter = "THA",
                partner = "all",
                trade_direction = "exports",
                start_date = 2007,
                end_date = 2011,
                commodity_code = shrimp_codes)

save(example_3, file = 'inst/extdata/vignette_data_3.rda')

#
# #
# example_2 <- comtradr::ct_get_data(
#   reporter = 'USA',
#   partner = c('DEU', 'FRA','JPN','MEX'),
#   commodity_code = ct_commodity_lookup("tomato",
#                                                        return_code = TRUE,
#                                                        return_char = TRUE),
#   start_date = "2012",
#   end_date = "2013",
#   flow_direction = 'import'
# )
#
# ct_get_data(
#   reporter = 'USA',
#   partner = c('DEU', 'FRA','JPN','MEX'),
#   commod_codes = c("0702", "070200", "2002", "200210", "200290"),
#   start_date = "2012",
#   end_date = "2013",
#   flow_direction = 'import'
# )
#
# save(example_2, file = 'inst/extdata/vignette_data_2.rda')
#
#
# comtradr:::ct_check_params(
#   type = 'goods',
#   reporter = 'USA',
#   partner = c('DEU', 'FRA', 'JPN', 'MEX'),
#   commodity_code = 'TOTAL',
#   commodity_classification = 'HS',
#   start_date = "2012",
#   end_date = "2012",
#   frequency = 'M',
#   flow_direction = 'import',
#   mode_of_transport = '0',
#   customs_code = 'C00',
#   partner_2 = 'World',
#   verbose = T,
#   update = F
# )
