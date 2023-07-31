#' Get trade data from the UN Comtrade API
#'
#'
#' @param frequency The frequency of returned trade data. A character value. Possible values are: 'A' for annual data and 'M' for monthly data. The default value is 'A'.
#' @param type The type of returned trade data. A character value. Possible values are: 'goods' for trade in goods and 'services' for trade in services. The default value is 'goods'.
#' @param commodity_classification The trade (IMTS) classification scheme. A character value. The possible values are `c('HS','S1','S2','S3','S4','SS')` for goods and `c('B4','B5','EB02','EB10','EB10S','EB')` for services.
#' @param commodity_code The commodity code(s). A character vector or `NULL`. All possible values are provided in the `comtradr::ct_get_ref_table('HS')` function (here for the HS commodity classification). You should use the relevant value from the `id` column. The default value is 'TOTAL': the sum of all commodities. If you provide `NULL`, all possible values are queried.
#' @param flow_direction The direction of trade flows. A character vector or `NULL`. Possible values are: 'import' for imports, 'export' for exports, 're-import' for re-imports, 're-export' for re-exports, or 'all' for imports, exports, re-imports, and re-exports. The default value is 'all'. If you provide `NULL`, all possible values are queried.
#' @param reporter Reporter ISO3 code(s). A character vector or `NULL`. Possible values include the desired country's ISO3 code. A full list of these can be found in the `comtradr::country_codes` dataset. 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries). The default value is 'all'. If you provide `NULL`, all possible values are queried.
#' @param partner Partner ISO3 code(s). A character vector or `NULL`. Possible values include the desired country's ISO3 code. A full list of these can be found in the `comtradr::country_codes` dataset. 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries). The default value is 'World' which returns the trade with all partner countries as an aggregate. If you provide `NULL`, all possible values are queried.
#' @param start_date The start date of the query. A character value. Yearly values should be in the format: `yyyy`. Monthly values should be in the format: `yyyy-mm`.
#' @param end_date The end date of the query. A character value. Yearly values should be in the format: `yyyy`. Monthly values should be in the format: `yyyy-mm`. This can be a maximum of 12 years after the start date for the annual data or one year after the start date for monthly data.
#' @param primary_token Your primary UN Comtrade API token. A character value. Default is to check in environment for stored token, if not passed through the `comtradr::set_primary_comtrade_key` function.
#' @param process A logical value. If TRUE, returns a data.frame with the results. If FALSE, returns the raw httr2 request. Defaults to TRUE.
#' @param verbose A logical value. If TRUE, sends status updates to the console. If FALSE, runs functions quietly.
#' @param update A logical value. If TRUE, will download the possibly updated reference tables from the UN.
#' @param mode_of_transport The Mode of Transport is set to `0`, which is the default for TOTAL across all modes of transportation. All possible values are provided in the function `ct_get_ref_table(dataset_id = 'mot')`. If you provide `NULL`, all possible values are queried
#' @param partner_2 Partner ISO3 code(s). A character vector or `NULL`. This value is set as a default to `World`, which is most likely the most general value and also the default on the Comtrade website. If you provide `NULL`, all possible values are queried
#' @param customs_code The customs code is set to the default of `C00` which is the default for TOTAL across all customs procedures. All possible values are provided in the function `ct_get_ref_table(dataset_id = 'customs'`. If you provide `NULL`, all possible values are queried.
#' @param ... You can pass in further parameters to the API that will not be checked and passed on as query parameters as is.

#' @examplesIf interactive()
#' ct_get_data(type = 'goods',
#' commodity_classification = 'HS',
#' commodity_code = 'TOTAL',
#' reporter = 'CHN',
#' partner = c('ARG','DEU'),
#' start_date = '2019',
#' end_date = '2019',
#' flow_direction = 'all',
#' partner_2 = 'World',
#' verbose = TRUE)
#'
#' @export
#' @return returns a data.frame with trade data or if `process = F` a httr2response object.
ct_get_data <- function(type = 'goods',
                        frequency = 'A',
                        commodity_classification = 'HS',
                        commodity_code = 'TOTAL',
                        flow_direction = 'all',
                        reporter = 'all',
                        partner = 'World',
                        start_date = NULL,
                        end_date = NULL,
                        process = TRUE,
                        verbose = FALSE,
                        primary_token = get_primary_comtrade_key(),
                        mode_of_transport = '0',
                        partner_2 = 'World',
                        customs_code ='C00',
                        update = FALSE,
                        ...) {
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
    includeDesc = "TRUE",
    update = update,
    ...
  )

  req <-
    ct_build_request(params, verbose = verbose, primary_token = primary_token)

  resp <- ct_perform_request(req, verbose = verbose)

  if (process) {
    result <- ct_process_response(resp, verbose = verbose)
    return(result)
  } else{
    return(resp)
  }
}
