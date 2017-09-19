#' UN Comtrade data extraction via JSON
#'
#' Function makes an API call to UN Comtrade, data is returned from the API as
#' a JSON object. Return value of this function is a list containing the data
#' as a dataframe and information on whether the connection to the API was
#' successful, and whether or not data was returned from the API. For use with
#' the UN Comtrade API, full API docs can be found at
#' \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param url Complete url string of the call to the API.
#' @param col_name Indication as to whether to use "human friendly" col names
#'  or "machine friendly" col names. Value will be either "H" or "M".
#'
#' @return List of length three, elements are:
#'  \itemize{
#'  \item \code{msg}: Brief message on success/failure of the API call.
#'  \item \code{details}: More detailed message on success/failure of the API
#'    call.
#'  \item \code{data}: Dataframe object of return data.
#'  }
#' @importFrom magrittr "%>%"
ct_json_data <- function(url, col_name) {

  res <- tryCatch(httr::GET(url, httr::user_agent(get("ua", envir = ct_env))),
                  error = function(e) e)

  if (methods::is(res, "error")) {
    msg <- "Could not complete connection to API"
    details <- NULL
    return(list(msg = msg, details = details, data = NULL))
  }

  if (httr::http_error(res)) {
    msg <- "Could not complete connection to API"
    details <- httr::status_code(res)
    return(list(msg = msg, details = details, data = NULL))
  }

  if (httr::http_type(res) != "application/json") {
    msg <- "API did not return a json object"
    details <- httr::http_type(res)
    return(list(msg = msg, details = details, data = NULL))
  }

  raw_data <- res %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyDataFrame = TRUE)

  if (length(raw_data$dataset) == 0) {
    msg <- raw_data$validation$status$name
    details <- raw_data$validation$message
    df <- NULL
  } else {
    if (col_name == "H") {
      colnames(raw_data$dataset) <- api_col_names()
    }
    msg <- "data_returned"
    details <- "connection_successful"
    df <- raw_data$dataset
  }

  return(list(msg = msg, details = details, data = df))
}
