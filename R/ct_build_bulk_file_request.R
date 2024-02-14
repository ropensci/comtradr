#' Build a valid request object from the checked parameters
#'
#' This is an internal function takes the necessary parameters
#' from `ct_check_params()` and creates a httr2 request to be performed.
#' This request can then be used in a second function, `ct_perform_request()`
#' to actually return the data. It is called internally ct `ct_get_bulk()`.
#'
#'
#' @noRd
#' @returns a httr2 request object
#'
ct_build_bulk_file_request <- function(reporter_code,
                                       file_hash,
                                       primary_token,
                                       verbose){
  req <- httr2::request("https://comtradeapi.un.org/bulk/v1/file/") |>
    httr2::req_url_path_append(list(reporter_code, file_hash)) |>
    httr2::req_url_query(format = "json") |>
    httr2::req_headers(`Ocp-Apim-Subscription-Key` = primary_token)

  if (stringr::str_length(req$url) > 4095) {
    rlang::abort("Your request exceeds 4KB or 4096 characters, which is the upper limit of the Comtrade API.") # nolint
  }

  if (verbose) {
    cli::cli_inform(c("i" = paste0("URL that will be queried: ", req$url)))
  }

  return(req)
}

