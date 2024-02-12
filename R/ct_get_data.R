#' Get trade data from the UN Comtrade API
#'
#' This function queries the UN Comtrade API to retrieve
#' international trade data.
#' It allows for detailed specification of the query,
#' including the type of data (goods or services),
#' frequency (annual or monthly), commodity classification,
#' flow direction, and more.
#' By providing `everything` for certain parameters,
#' you can query all possible values.
#' The function is opinionated in that it already verifies certain parameters
#' for you and is more than a pure wrapper around the API.
#'
#' @details
#' The UN Comtrade database provides a repository of official international
#' trade statistics and relevant analytical tables.
#' It contains annual trade statistics starting from 1988
#' and monthly trade statistics since 2000 for goods data
#'
#' Parameters that accept `everything` will query all possible values.
#' For example, setting `commodity_code = 'everything'`
#' will retrieve data for all commodity codes.
#' This can be useful for broad queries but may result in large datasets.
#'
#' @param frequency The frequency of returned trade data.
#' Possible values: 'A' for annual data, 'M' for monthly data. Default: 'A'.
#' @param type The type of returned trade data.
#' Possible values: 'goods' for trade in goods,
#' 'services' for trade in services. Default: 'goods'.
#' @param commodity_classification The trade classification scheme.
#' Possible values for goods: `c('HS','S1','S2','S3','S4','SS','B4','B5')`;
#' for services: `c('EB02','EB10','EB10S','EB')`. Default: 'HS'.
#' @param commodity_code The commodity code(s) or `everything` for all possible
#' codes. See `comtradr::ct_get_ref_table('HS')` for possible values.
#' Default: 'TOTAL' (sum of all commodities).
#' @param flow_direction The direction of trade flows or `everything`.
#' Possible values can be found in `ct_get_ref_table('flow_direction')`. These
#' are implemented case-insensitive, 'import' and 'Import' are equivalent.
#' Default: c('import','export','re-export','re-import').
#' @param reporter Reporter ISO3 code(s), `everything` or `all_countries`.
#' See `comtradr::country_codes` or `comtradr::ct_get_ref_table('reporter')`
#' for possible values. `all_countries` returns all countries without aggregates
#' `everything` returns all possible parameters. Default: 'all_countries'.
#' @param partner Partner ISO3 code(s), `everything` or `all_countries`.
#' See `comtradr::country_codes` for possible values.
#' `all_countries` returns all countries without aggregates
#' `everything` returns all possible parameters, incl. aggregates like World.
#' Default: 'World' (all partners as an aggregate).
#' @param start_date The start date of the query.
#' Format: `yyyy` for yearly, `yyyy-mm` for monthly.
#' @param end_date The end date of the query.
#' Format: `yyyy` for yearly, `yyyy-mm` for monthly.
#' Max: 12 years after start date for annual data, one year for monthly data.
#' @param primary_token Your primary UN Comtrade API token.
#' Default: stored token from `comtradr::set_primary_comtrade_key`.
#' @param process If TRUE, returns a data.frame with results.
#' If FALSE, returns the raw httr2 request. Default: TRUE.
#' @param tidy_cols If TRUE, returns tidy column names.
#' If FALSE, returns raw column names. Default: TRUE.
#' @param verbose If TRUE, sends status updates to the console.
#' If FALSE, runs functions quietly. Default: FALSE.
#' @param mode_of_transport Text code of mode of transport or `everything` for
#' all possible parameters.
#' See `ct_get_ref_table(dataset_id = 'mode_of_transport')` for possible values.
#' Default: 'TOTAL modes of transport' (TOTAL).
#' @param partner_2 Partner 2 ISO3 code(s), `everything` or `all_countries`.
#' See `comtradr::country_codes` for possible values.
#' `all_countries` returns all countries without aggregates
#' `everything` returns all possible parameters, incl. aggregates like World.
#' Default: 'World' (all partners as an aggregate).
#' @param customs_code Customs Code ID or `everything` for all possible
#' parameters.
#' See `ct_get_ref_table(dataset_id = 'customs_code')` for possible values.
#' Default: 'C00' (TOTAL).
#' @param update If TRUE, downloads possibly updated reference tables
#' from the UN. Default: FALSE.
#' @param requests_per_second Rate of requests per second executed,
#' usually specified as a fraction, e.g. 10/60 for 10 requests per minute,
#' see `req_throttle()` for details.
#' @param cache A logical value to determine, whether requests should be cached
#' or not. If set to True, `rappdirs::user_cache_dir()` is used
#' to determine the location of the cache. Use the .Renviron file to set the
#' R_USER_CACHE_DIR in order to change this location. Default: False.
#' @param extra_params Additional parameters to the API, passed as query
#' parameters without checking. Please provide a named list to this parameter.
#' Default: NULL.
#'
#' @examplesIf interactive()
#' # Query goods data for China's trade with Argentina and Germany in 2019
#' ct_get_data(
#'   type = "goods",
#'   commodity_classification = "HS",
#'   commodity_code = "TOTAL",
#'   reporter = "CHN",
#'   partner = c("ARG", "DEU"),
#'   start_date = "2019",
#'   end_date = "2019",
#'   flow_direction = "Import",
#'   partner_2 = "World",
#'   verbose = TRUE
#' )
#'
#' # Query all commodity codes for China's imports from Germany in 2019
#' ct_get_data(
#'   commodity_code = "everything",
#'   reporter = "CHN",
#'   partner = "DEU",
#'   start_date = "2019",
#'   end_date = "2019",
#'   flow_direction = "Import"
#' )
#'
#' # Query all commodity codes for China's imports from Germany
#' # from January to June of 2019
#' ct_get_data(
#'   commodity_code = "everything",
#'   reporter = "CHN",
#'   partner = "DEU",
#'   start_date = "2019",
#'   end_date = "2019",
#'   flow_direction = "import"
#' )
#'
#' @export
#' @returns A data.frame with trade data or,
#' if `process = F`, a httr2 response object.

ct_get_data <- function(type = "goods",
                        frequency = "A",
                        commodity_classification = "HS",
                        commodity_code = "TOTAL",
                        flow_direction = c(
                          "Import", "Export",
                          "Re-export", "Re-import"
                        ),
                        reporter = "all_countries",
                        partner = "World",
                        start_date = NULL,
                        end_date = NULL,
                        process = TRUE,
                        tidy_cols = TRUE,
                        verbose = FALSE,
                        primary_token = get_primary_comtrade_key(),
                        mode_of_transport = "TOTAL modes of transport",
                        partner_2 = "World",
                        customs_code = "C00",
                        update = FALSE,
                        requests_per_second = 10 / 60,
                        extra_params = NULL,
                        cache = FALSE) {
  ## compile codes
  params <- ct_check_params(
    type = type,
    frequency = frequency,
    commodity_classification = commodity_classification,
    commodity_code = commodity_code,
    flow_direction = flow_direction,
    partner = partner,
    reporter = reporter,
    start_date = start_date,
    end_date = end_date,
    verbose = verbose,
    mode_of_transport = mode_of_transport,
    partner_2 = partner_2,
    customs_code = customs_code,
    update = update,
    extra_params = extra_params
  )

  req <-
    ct_build_request(params,
      verbose = verbose,
      primary_token = primary_token
    )

  if(cache){
    resp <- ct_perform_request_cache(req,
                               requests_per_second = requests_per_second,
                               verbose = verbose
    )
  } else{
    resp <- ct_perform_request(req,
                               requests_per_second = requests_per_second,
                               verbose = verbose
    )
  }

  if (process) {
    result <- ct_process_response(resp,
      verbose = verbose,
      tidy_cols = tidy_cols
    )
    return(result)
  } else {
    return(resp)
  }
}

