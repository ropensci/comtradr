#' Build a valid request object from the checked parameters
#'
#' This is an internal function takes the necessary parameters from `ct_check_params()`
#' and creates a httr2 request to be performed. This request can then be used in a second function, `ct_perform_request()` to actually return the data.
#' It is called internally ct `ct_get_data()`
#'
#' @param params a named vector of parameters for the comtrade request, result from `ct_check_params()`.
#'
#' @examplesIf interactive()
#' # Build request from checked parameters
#' comtradr:::ct_build_request(comtradr:::ct_check_params(type = 'goods',
#'                                             frequency = 'A',
#'                                             commodity_classification = 'HS',
#'                                             commodity_code = 'TOTAL',
#'                                             flow_direction = 'all',
#'                                             reporter = 'all',
#'                                             partner = 'World',
#'                                             start_date = 2020,
#'                                             end_date = 2022,
#'                                             process = TRUE,
#'                                             tidy_cols = TRUE,
#'                                             verbose = FALSE,
#'                                             primary_token = 'xxxx',
#'                                             mode_of_transport = '0',
#'                                             partner_2 = 'World',
#'                                             customs_code ='C00',
#'                                             update = FALSE ))
#'
#' @returns a httr2 request object
#' @inheritParams ct_get_data
ct_build_request <- function(params,
                                   primary_token = NULL,
                                   verbose = FALSE) {
  query_params <- params$query_params

  type <- params$url_params$type

  freq <- params$url_params$freq

  clCode <- params$url_params$clCode

  res <-
    httr2::request("https://comtradeapi.un.org/data/v1/get/") |>
    httr2::req_url_path_append(type) |>
    httr2::req_url_path_append(freq) |>
    httr2::req_url_path_append(clCode) |>
    httr2::req_headers(`Ocp-Apim-Subscription-Key` = primary_token) |>
    httr2::req_url_query(!!!query_params)

  if(stringr::str_length(res$url)>4095){
    rlang::abort("Your request exceeds 4KB or 4096 characters, which is the upper limit of the Comtrade API.")
  }

  if (verbose) {
    cli::cli_inform(c("i" = paste0("URL that will be queried: ",res$url)))
  }

  return(res)
}
