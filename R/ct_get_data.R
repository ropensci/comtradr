#' Get trade data from the UN Comtrade API
#'
#' As the package is under development, please note, that the following parameters will be supplied internally in a fixed manner:
#' * customsCode is set to C00, which is the most general
#' * motCode is set to 0, which is all modes of transportation
#' * partner2Code is set to 0, which is the most general settings returning all data
#'
#' @param frequency The frequency of returned trade data. A character value. Possible values are: 'A' for annual data and 'M' for monthly data. The default value is 'A'.
#' @param commodity_classification The trade (IMTS) classification scheme. A character value. The only possible value is 'HS'. This is the default.
#' @param commodity_code The commodity code(s). A character vector. All possible values are provided in the `comtradr::cmd_codes` dataset. You should use the relevant value from the `id` column. The default value is 'TOTAL': the sum of all commodities.
#' @param flow_direction The direction of trade flows. A character vector. Possible values are: 'import' for imports, 'export' for exports, 're-import' for re-imports, 're-export' for re-exports, or 'all' for imports, exports, re-imports, and re-exports. The default value is 'all'.
#' @param reporter Reporter ISO3 code(s). A character vector. Possible values include the desired country's ISO3 code. A full list of these can be found in the `comtradr::country_codes` dataset. NULL can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries). The default value is NULL.
#' @param partner Partner ISO3 code(s). A character vector. Possible values include the desired country's ISO3 code. A full list of these can be found in the `comtradr::country_codes` dataset. NULL can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries). The default value is NULL.
#' @param start_date The start date of the query. A character value. Yearly values should be in the format: `yyyy`. Monthly values should be in the format: `yyyy-mm`.
#' @param end_date The end date of the query. A character value. Yearly values should be in the format: `yyyy`. Monthly values should be in the format: `yyyy-mm`. This can be a maximum of 12 years after the start date for the annual data or one year after the start date for monthly data.
#' @param primary_token Your primary UN Comtrade API token. A character value. Default is to check in environment for stored token, if not passed through the `comtradr::set_primary_comtrade_key` function.
#' @param process A logical value. If TRUE, returns a data.frame with the results. If FALSE, returns the raw httr2 request. Defaults to TRUE.
#' @param verbose A logical value. If TRUE, sends status updates to the console. If FALSE, runs functions quietly.
#' @param ... You can pass in further parameters to the API that will not be checked and passed on as query parameters as is.
#' @param mode_of_transport The Mode of Transport is set to `0`, which is the default for TOTAL across all modes of transportation. This parameter is so far not being validated.
#' @param partner_2 This value is set as a default to `0`, which is most likely the most general value and also the default on the Comtrade website.
#' @param customs_code The customs code is set to the default of `C00` which is the default for TOTAL across all customs procedures.
#'
#' @examplesIf interactive()
#' get_comtrade_data(frequency = 'A',
#' commodity_classification = 'HS',
#' commodity_code = c('2204','2203'),
#' flow_direction = 'export',
#' reporter = c("ARG","GBR"),
#' partner = 'world',
#' start_date = "2018",
#' end_date = "2019",
#' process = T)
#'
#' @export
#' @return returns a data.frame with trade data or if `process = F` returns a httr2response object.
ct_get_data <- function(frequency = 'A',
                        commodity_classification = 'HS',
                        commodity_code = 'TOTAL',
                        flow_direction = 'all',
                        reporter = NULL,
                        partner = NULL,
                        start_date = NULL,
                        end_date = NULL,
                        process = T,
                        verbose = F,
                        primary_token = get_primary_comtrade_key(),
                        mode_of_transport = '0',
                        partner_2 = '0',
                        customs_code ='C00',
                        ...) {
  ## compile codes
  params <- ct_check_params(
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
    ...
  )

  req <- ct_build_request(params, verbose = verbose, primary_token = primary_token)

  resp <- ct_perform_request(req, verbose = verbose)

  if (process) {
    result <- ct_process_response(resp, verbose = verbose)
    return(result)
  } else{
    return(resp)
  }
}
