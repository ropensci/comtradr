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
ct_perform_request <- function(req, requests_per_second = 10 / 60, verbose = FALSE) {

    if (verbose) {
      cli::cli_inform(c("i" = "Performing request, which can take a few seconds, depending on the amount of data queried."))
    }

    comtrade_is_transient <- function(resp) {
      httr2::resp_status(resp) == 403 &&
        httr2::resp_header(resp, "Retry-After") != "0"
    }

    comtrade_after <- function(resp) {
      time <- as.numeric(httr2::resp_header(resp, "Retry-After"))
    }

    resp <- req |>
      httr2::req_error(body = comtrade_error_body) |>
      httr2::req_throttle(rate = requests_per_second) |>
      httr2::req_retry(is_transient = comtrade_is_transient,
                       after = comtrade_after) |>
      httr2::req_perform()

    if (verbose) {
      cli::cli_inform(c("v" = "Got a response object from UN Comtrade. Use `process = F` if there is an error after this step to find issues with the response object."))
    }

    return(resp)
  }

comtrade_error_body <- function(resp) {
  if (stringr::str_detect(httr2::resp_header(resp, 'Content-Type'), 'json')) {
    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    message <- body$errorObject$errorMessage
    if (!is.null(message)) {
      message <- c(message)
    }
    return(message)
  } else if (stringr::str_detect(httr2::resp_header(resp,
                                                    'Content-Type'),
                                 'text')) {
    body <- httr2::resp_body_string(resp)

    if (stringr::str_detect(body, 'Request URL Too Long')) {
      message <-
        c('You might have provided too many parameters and the URL got too long.')
      return(message)
    } else if (stringr::str_detect(body,
                                   'The resource you are looking for has been removed')) {
      message <-
        c(
          'The original message is: ',
          body,
          'But most likely you have exceeded the character value for the api.'
        )
      return(message)
    }
  }
}
