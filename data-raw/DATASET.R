library(httr2)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(usethis)

# getting comtrade data ---------------------------------------------------

## getting list of reference tables
response <- httr2::request('https://comtradeapi.un.org/files/v1/app/reference/ListofReferences.json') |> # nolint
  httr2::req_perform()

## getting data from response of list of tables
list_of_datasets <- response |>
  httr2::resp_body_json(simplifyVector = T) |>
  purrr::pluck(1)

## getting date of last modification from list of tables
last_modified <- httr2::resp_header(header = "Last-Modified",
                                    resp = response) |>
  stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
  as.Date(format = "%d %b %Y")

## writing last modification to data
list_of_datasets$last_modified <- last_modified

## changing colon to underscore in dataset names
list_of_datasets$category <- stringr::str_replace_all(list_of_datasets$category,
                                                      ':',"_") |>
  tolower()

## save list of datasets
save(list_of_datasets, file = 'inst/extdata/list_of_datasets.rda')

## loop over all datasets (for loop, because it is readable,
## no need for speeding this up with more complicated lapply
## or more dependencies)
for(i in seq_along(list_of_datasets$category)){
  ## define the valid commodity codes that we need
  valid_cmd_datasets <-
    c(
      'cmd_hs',
      'cmd_h0',
      'cmd_h1',
      'cmd_h2',
      'cmd_h3',
      'cmd_h4',
      'cmd_h5',
      'cmd_h6',
      'cmd_s1',
      'cmd_s2',
      'cmd_s3',
      'cmd_s4',
      'cmd_ss',
      'cmd_b4',
      'cmd_b5',
      'cmd_eb02',
      'cmd_eb10',
      'cmd_eb10s',
      'cmd_eb'
    )

  valid_country_datasets <- c('reporter','partner')
  valid_other_datasets <- c('mot','customs','flow', 'freq','mos','qtyunit','dataitem')

  ## if it is a valid dataset that we need, download it
  if(list_of_datasets$category[i] %in% valid_cmd_datasets){
    response <- httr2::request(list_of_datasets$fileuri[i]) |>
      httr2::req_perform()

    data <- response |>
      httr2::resp_body_json(simplifyVector = T)

    last_modified <- httr2::resp_header(header = "Last-Modified",
                                        resp = response) |>
      stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
      as.Date(format = "%d %b %Y")

    result <- data$results

    result$last_modified <- last_modified

    readr::write_rds(result, "xz",
                     file = paste0('inst/extdata/',
                                   list_of_datasets$category[i],'.rds'))
  } else if(list_of_datasets$category[i] %in% valid_country_datasets) {
    response <- httr2::request(list_of_datasets$fileuri[i]) |>
      httr2::req_perform()

    data <- response |>
      httr2::resp_body_json(simplifyVector = T)

    last_modified <- httr2::resp_header(header = "Last-Modified",
                                        resp = response) |>
      stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
      as.Date(format = "%d %b %Y")

    if(list_of_datasets$category[i]=='reporter'){
      result <- data$results |>
        dplyr::transmute(
          id,
          country = text,
          iso_3 = reporterCodeIsoAlpha3,
          iso_2 = reporterCodeIsoAlpha2,
          note = reporterNote,
          entry_year = lubridate::year(entryEffectiveDate),
          exit_year = lubridate::year(entryExpiredDate),
          group = isGroup
        )
    } else {
      result <- data$results |>
        dplyr::transmute(
          id,
          country = text,
          iso_3 = PartnerCodeIsoAlpha3,
          iso_2 = PartnerCodeIsoAlpha2,
          note = partnerNote,
          entry_year = lubridate::year(entryEffectiveDate),
          exit_year = lubridate::year(entryExpiredDate),
          group = isGroup
        ) |>
        dplyr::mutate(iso_3 = ifelse(country=='World','World',iso_3))

    }

    result$last_modified <- last_modified

    readr::write_rds(result, "xz",
                     file = paste0('inst/extdata/',
                                   list_of_datasets$category[i],'.rds'))
  } else if(list_of_datasets$category[i] %in% valid_other_datasets) {
    response <- httr2::request(list_of_datasets$fileuri[i]) |>
      httr2::req_perform()

    data <- response |>
      httr2::resp_body_json(simplifyVector = T)

    last_modified <- httr2::resp_header(header = "Last-Modified",
                                        resp = response) |>
      stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
      as.Date(format = "%d %b %Y")

    result <- data$results

    result$last_modified <- last_modified

    readr::write_rds(result, "xz",
                     file = paste0('inst/extdata/',
                                   list_of_datasets$category[i],'.rds'))
  } else {
    next
  }
}

# Consolidate datasets ----------------------------------------------------
reporter_codes <- readr::read_rds(paste0('inst/extdata/','reporter','.rds')) |>
  dplyr::mutate(reporter =T)
partner_codes <- readr::read_rds(paste0('inst/extdata/','partner','.rds'))|>
  dplyr::mutate(partner =T)
country_codes <- dplyr::full_join(reporter_codes, partner_codes)

# Save external datasets --------------------------------------------------

usethis::use_data(country_codes, overwrite = TRUE)



ct_pretty_cols <- data.frame(
  to = c(
    'type_code',
    'freq_code',
    'ref_period_id',
    'ref_year',
    'ref_month',
    'period',
    'reporter_code',
    'reporter_iso',
    'reporter_desc',
    'flow_code',
    'flow_desc',
    'flow_category',
    'partner_code',
    'partner_iso',
    'partner_desc',
    'partner2code',
    'partner2iso',
    'partner2desc',
    'classification_code',
    'classification_search_code',
    'is_original_classification',
    'cmd_code',
    'cmd_desc',
    'aggr_level',
    'is_leaf',
    'customs_code',
    'customs_desc',
    'mos_code',
    'mos_desc',
    'mot_code',
    'mot_desc',
    'qty_unit_code',
    'qty_unit_abbr',
    'qty',
    'is_qty_estimated',
    'alt_qty_unit_code',
    'alt_qty_unit_abbr',
    'alt_qty',
    'is_alt_qty_estimated',
    'net_wgt',
    'is_net_wgt_estimated',
    'is_net_wgt_estimated',
    'gross_wgt',
    'is_gross_wgt_estimated',
    'is_gross_wgt_estimated',
    'cifvalue',
    'cifvalue',
    'fobvalue',
    'fobvalue',
    'primary_value',
    'legacy_estimation_flag',
    'is_reported',
    'is_aggregate',
    "dataset_code",
    "cifvalue",
    "fobvalue"
  ), from = c(
    "typeCode",
    "freqCode",
    "refPeriodId",
    "refYear",
    "refMonth",
    "period",
    "reporterCode",
    "reporterISO",
    "reporterDesc",
    "flowCode",
    "flowDesc",
    "flowCategory",
    "partnerCode",
    "partnerISO",
    "partnerDesc",
    "partner2Code",
    "partner2ISO",
    "partner2Desc",
    "classificationCode",
    "classificationSearchCode",
    "isOriginalClassification",
    "cmdCode",
    "cmdDesc",
    "aggrLevel",
    "isLeaf",
    "customsCode",
    "customsDesc",
    "mosCode",
    "mosDesc",
    "motCode",
    "motDesc",
    "qtyUnitCode",
    "qtyUnitAbbr",
    "qty",
    "isQtyEstimated",
    "altQtyUnitCode",
    "altQtyUnitAbbr",
    "altQty",
    "isAltQtyEstimated",
    "netWgt",
    "isNetWgtEstimated",
    "isnetWgtEstimated",
    "grossWgt",
    "isGrossWgtEstimated",
    "isgrossWgtEstimated",
    "cifvalue",
    "cifValue",
    "fobvalue",
    "fobValue",
    "primaryValue",
    "legacyEstimationFlag",
    "isReported",
    "isAggregate",
    "datasetCode",
    "CIFValue",
    "FOBValue"
  ))


usethis::use_data(ct_pretty_cols, overwrite = TRUE)






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
                flow_direction = "exports",
                start_date = 2007,
                end_date = 2011,
                commodity_code = shrimp_codes)

save(example_3, file = 'inst/extdata/vignette_data_3.rda')



### vignette for large data files
data_eu_imports <- data.frame()

for(reporter in eu_countries){
  ## for a simple status, print the country we are at
  ## you can get a lot fancier with the library `progress` for progress bars
  print(reporter)

  ## assign the result into a temporary object
  temp <- ct_get_data(
    commodity_code = wood,
    reporter = reporter,
    partner = "all_countries",
    flow_direction = "import",
    start_date = 2018,
    end_date = 2022
  )

  ## bind the subset to the complete data
  data_eu_imports <- rbind(data_eu_imports, temp)

  ## note that I did not include any sleep() command here to make the requests
  ## wait for a specified amount of time, the package keeps track of that for
  ## you automatically and backs off when needed
}

data_eu_imports <- data_eu_imports|>
  select(
    reporter_iso,
    reporter_desc,
    flow_desc,
    partner_iso,
    partner_desc,
    cmd_code,
    cmd_desc,
    primary_value,
    ref_year
  )


save(data_eu_imports, file = 'inst/extdata/vignette_data_4.rda')


data_eu_imports_world <- ct_get_data(
  commodity_code = wood,
  reporter = eu_countries,
  partner = "World",
  flow_direction = "import",
  start_date = 2018,
  end_date = 2022
)

save(data_eu_imports_world, file = 'inst/extdata/vignette_data_5.rda')


eu_countries <- giscoR::gisco_countrycode |>
  filter(eu == T) |>
  pull(ISO3_CODE)

save(eu_countries, file = 'inst/extdata/vignette_data_6.rda')




#### vignette for transition

q <- ct_get_data(reporter = 'USA',
                 partner = c("DEU"),
                 flow_direction = "import",
                 start_date = 2012,
                 end_date = 2012,
                 frequency = "A")
save(q, file = 'inst/extdata/vignette_data_7.rda')

hs0 <- comtradr::ct_get_data(
  reporter = c("DEU","FRA"), # only some examples here,
  commodity_classification = 'HS',
  commodity_code = '0306',
  start_date = 1990, # only one year here
  end_date = 1990)

save(hs0, file = 'inst/extdata/vignette_data_8.rda')


hs5 <- comtradr::ct_get_data(
  reporter = c("DEU","FRA"), # only some examples here,
  commodity_classification = 'HS',
  commodity_code = '0306',
  start_date = 2020, # only one year here
  end_date = 2020)

save(hs5, file = 'inst/extdata/vignette_data_9.rda')


hs0_all <- comtradr::ct_get_bulk(
  reporter = c("ARG"), # only some examples here,
  commodity_classification = 'H0',
  frequency = 'A',
  verbose = T,
  start_date = 2000, # only one year here
  end_date = 2000) |>
  dplyr::slice(1:100)

save(hs0_all, file = 'inst/extdata/vignette_data_10.rda')


# Data for trade matrix vignette ------------------------------------------

## The trade matrix vignette displays REAL output from the estimated trade
## matrix endpoint. The full 2022 world section-0 export matrix is ~18,690
## rows (~5 MB), which is far too large to ship. Instead we bake a handful of
## small objects computed over the full response. This file is documentation
## only; it is not run on package build.
##
## The API token lives in the project `.env` file under the variable name
## `key` (NOT the usual `COMTRADE_PRIMARY`). Load it locally and pass it
## explicitly. Never print, log or commit the key value.
readRenviron(".env")

## One live pull: the complete world export matrix for SITC section "0"
## (food and live animals), 2022, reporter/partner both "everything" so the
## response includes UN estimates for non-reporting countries.
##
## `include_world = TRUE` keeps the aggregate World rows so we can measure the
## double-counting hazard below. Everything else is computed on the bilateral
## rows only. See .agent/trade_matrix_api_ground_truth.md.
res_all <- ct_get_trade_matrix(
  commodity_code = "0",
  flow_direction = "export",
  reporter = "everything",
  partner = "everything",
  start_date = 2022,
  end_date = 2022,
  include_world = TRUE,
  primary_token = Sys.getenv("key")
)

## The bilateral matrix: what `include_world = FALSE` (the default) returns.
res <- res_all |>
  dplyr::filter(reporter_code != 0, partner_code != 0)

## `trade_matrix_blocks`: why the World rows matter. The bilateral flows, the
## reporter margins, the partner margins and the grand total each sum to the
## same world total, so summing the raw response over-counts fourfold.
trade_matrix_blocks <- res_all |>
  dplyr::mutate(
    block = dplyr::case_when(
      reporter_code == 0 & partner_code == 0 ~ "World / World (grand total)",
      reporter_code == 0 ~ "reporter = World (column margin)",
      partner_code == 0 ~ "partner = World (row margin)",
      .default = "bilateral flows"
    )
  ) |>
  dplyr::group_by(block) |>
  dplyr::summarise(
    n_rows = dplyr::n(),
    total_value = sum(primary_value, na.rm = TRUE)
  ) |>
  as.data.frame()

## `trade_matrix_sample`: a small illustrative slice for showing structure.
## We keep only display columns and take one coherent reporter's exports
## (Montenegro) that mixes reported and estimated partner flows, so the
## sample contains BOTH is_reported == TRUE and is_reported == FALSE rows.
## Bilateral rows only -- the World margin row is excluded by `res`.
display_cols <- c(
  "reporter_iso", "reporter_desc", "partner_iso", "partner_desc",
  "flow_desc", "cmd_code", "cmd_desc", "primary_value", "ref_year",
  "classification_code", "is_reported"
)

trade_matrix_sample <- res |>
  dplyr::filter(reporter_iso == "MNE") |>
  dplyr::select(dplyr::all_of(display_cols)) |>
  dplyr::arrange(reporter_iso, dplyr::desc(primary_value)) |>
  as.data.frame()

## `trade_matrix_totals`: the reported-vs-estimated split of world section-0
## exports for 2022, computed on the BILATERAL rows of the full response.
## Note this is a split by cell provenance, not "extra coverage": a cell is
## flagged FALSE whenever it passed through the UNSD estimation pipeline,
## which happens to full reporters too.
trade_matrix_totals <- res |>
  dplyr::group_by(is_reported) |>
  dplyr::summarise(
    total_value = sum(primary_value, na.rm = TRUE),
    n_flows = dplyr::n()
  ) |>
  as.data.frame()

## `trade_matrix_coverage`: the coverage story, which is the real reason to
## reach for this endpoint. A country that did not report at all in a given
## year has is_reported == FALSE on every one of its rows.
reporter_coverage <- res |>
  dplyr::group_by(reporter_iso, reporter_desc) |>
  dplyr::summarise(
    any_reported = any(is_reported),
    total_value = sum(primary_value, na.rm = TRUE),
    .groups = "drop"
  )

trade_matrix_coverage <- data.frame(
  reporters_total = nrow(reporter_coverage),
  reporters_not_reporting = sum(!reporter_coverage$any_reported),
  value_share_not_reporting = sum(
    reporter_coverage$total_value[!reporter_coverage$any_reported]
  ) / sum(reporter_coverage$total_value)
)

## `trade_matrix_nonreporters`: the ten largest exporters that filed nothing
## for 2022, i.e. the countries `ct_get_data()` would silently omit.
trade_matrix_nonreporters <- reporter_coverage |>
  dplyr::filter(!any_reported) |>
  dplyr::arrange(dplyr::desc(total_value)) |>
  dplyr::slice(1:10) |>
  dplyr::select(reporter_iso, reporter_desc, total_value) |>
  as.data.frame()

## Drop the httr2 response metadata (`url`/`time`) that the dplyr pipeline
## inherits from the API result, so the shipped objects are clean data frames
## and `str()` in the vignette stays tidy.
tm_objects <- c(
  "trade_matrix_sample", "trade_matrix_totals", "trade_matrix_blocks",
  "trade_matrix_coverage", "trade_matrix_nonreporters"
)
for (obj in tm_objects) {
  x <- get(obj)
  for (a in c("url", "time")) {
    attr(x, a) <- NULL
  }
  assign(obj, x)
}

## Save all of them into one file with xz compression (well under 50 KB).
save(
  list = tm_objects,
  file = "inst/extdata/vignette_data_trade_matrix.rda",
  compress = "xz"
)


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
