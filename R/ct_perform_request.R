#' Performs the request to the Comtrade API
#'
#' Performs the request and returns an error body with the respective error returned by the Comtrade API. Also throttles all requests to 1 request per 6 seconds, or 10 requests per minute.
#'
#' @param req a valid comtrade request built by the `ct_build_request()` function
#' @param requests_per_second rate at which throttling is done
#'
#' @param verbose whether the function sends status updates to the console
#'
#' @return json data from comtrade and possible error codes
ct_perform_request <- function(req, requests_per_second = 10 / 60, verbose = F) {

    if (verbose) {
      cli::cli_inform(c("i" = "Performing request, which can take a few seconds, depending on the amount of data queried"))
    }

    resp <- req |>
      httr2::req_error(body = comtrade_error_body) |>
      httr2::req_throttle(rate = requests_per_second) |>
      httr2::req_retry(max_tries = 5) |>
      httr2::req_perform()

    if (verbose) {
      cli::cli_inform(c("v" = "Got a response object from Comtrade. Use `process = F` if there is an error after this step to find issues with the response object."))
    }
    return(resp)
  }

comtrade_error_body <- function(resp) {
  body <- httr2::resp_body_json(resp, simplifyVector = T)

  message <- body$errorObject$errorMessage
  if (!is.null(message)) {
    message <- c(message)
  }
  message
}
