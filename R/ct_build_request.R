#' Build a valid request object from the checked parameters
#'
#' This function takes the necessary parameters and creates a httr2 request to be performed this request can then be used in a second function, to actually return the data
#'
#' @param params a named vector of parameters for the comtrade request
#'
#' @param primary_token Your primary token. Default is to check in environment for stored token, if not passed through the `set_primary_comtrade_key` function
#'
#' @param verbose whether the function sends status updates to the console
#'
#' @return a httr2 request object
ct_build_request <- function(params,
                                   primary_token = NULL,
                                   verbose = F) {
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
