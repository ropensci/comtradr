#' UN Comtrade data extraction via CSV
#'
#' Function makes an API call to UN Comtrade, data is returned from the API as
#' a CSV object. Return value of this function is a list containing the data
#' as a dataframe and information on whether the connection to the API was
#' successful, and whether or not data was returned from the API. For use with
#' the UN Comtrade API, full API docs can be found at
#' \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param url Complete url string of the call to the API.
#' @param colname Indication as to whether to use "human friendly" col names or
#'  "machine friendly" col names. Value will be either "H" or "M".
#'
#' @return List of length three, elements are:
#'  \itemize{
#'  \item \code{msg}: Brief message on whether data was returned.
#'  \item \code{details}: Further details related to the nature of the API
#'    return.
#'  \item \code{data}: Dataframe object of return data.
#'  }
ct_csv_data <- function(url, colname) {

  rawdata <- tryCatch(utils::read.csv(url, stringsAsFactors = FALSE),
                      error = function(e) e)

  if (methods::is(rawdata, "error")) {
    msg <- "No data returned"
    details <- "Could not complete connection to API"
    return(list(msg = msg, details = details, data = NULL))
  }

  if (grepl("No data matches", rawdata$Classification[1],
            ignore.case = TRUE)) {
    msg <- "No data returned"
    details <- "Connection successful"
    return(list(msg = msg, details = details, data = NULL))
  }

  if (ncol(rawdata) == 1) {
    msg <- "No data returned, param error within API function call"
    details <- rawdata[[1]][1]
    return(list(msg = msg, details = details, data = NULL))
  }

  if (colname == "H") {
    colnames(rawdata) <- api_col_names()
  }
  msg <- "Data returned"
  details <- "Connection successful"
  return(list(msg = msg, details = details, data = rawdata))
}
