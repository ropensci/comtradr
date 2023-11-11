#' Performs the request to the Comtrade API
#'
#' This function is internally called by `ct_get_data()` and performs the request constructed by `ct_build_request()` and returns an error body with the respective error returned by the Comtrade API.
#' By default throttles all requests to 1 request per 6 seconds, or 10 requests per minute, but it adjusts automatically if the
#' API asks for longer waiting times.
#'
#' @param req a valid comtrade request built by the `ct_build_request()` function
#'
#' @returns JSON data from comtrade, data.frame with results or error codes.
#' @examplesIf interactive()
#' ct_get_data(commodity_code = NULL,
#'             reporter = 'CHN',
#'             partner = 'DEU',
#'             start_date = '2019',
#'             end_date = '2019',
#'             flow_direction = 'import')
#' req <- httr2::last_request()
#' resp <- ct_perform_request(req, requests_per_second = 10/60, verbose = FALSE)
#' @inheritParams ct_get_data
ct_perform_request <- function(req, requests_per_second, verbose = FALSE) {

    if (verbose) {
      cli::cli_inform(c("i" = "Performing request, which can take a few seconds, depending on the amount of data queried."))
    }

    comtrade_is_transient <- function(resp) {
      (httr2::resp_status(resp) == 403 &&
        httr2::resp_header(resp, "Retry-After") != "0") ||
      (httr2::resp_status(resp) == 429 &&
        httr2::resp_header(resp, "Retry-After") != "0")
    }

    comtrade_after <- function(resp) {
      time <- as.numeric(httr2::resp_header(resp, "Retry-After"))
      time
    }

    resp <- req |>
      httr2::req_error(body = comtrade_error_body) |>
      httr2::req_throttle(rate = requests_per_second) |>
      httr2::req_retry(is_transient = comtrade_is_transient,
                       after = comtrade_after,
                       max_tries = 2) |>
      httr2::req_perform()

    if (verbose) {
      cli::cli_inform(c("v" = "Got a response object from UN Comtrade. Use `process = F` if there is an error after this step to find issues with the response object."))
    }

    return(resp)
  }

comtrade_error_body <- function(resp) {
  if (!is.null(httr2::resp_header(resp, 'Content-Type'))) {
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

}
