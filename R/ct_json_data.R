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
#' @param colname Indication as to whether to use "human friendly" col names or 
#'  "machine friendly" col names. Value will be either "H" or "M".
#'
#' @return List of length three, elements are:
#'  \itemize{
#'  \item \code{msg}: Brief message on success/failure of the API call.
#'  \item \code{details}: More detailed message on success/failure of the API 
#'    call.
#'  \item \code{data}: Dataframe object of return data.
#'  }
#' @importFrom dplyr "%>%"
#' @export
ct_json_data <- function(url, colname) {
  
  rawdata <- tryCatch(httr::GET(url), error = function(e) e)
  
  if (is(rawdata, "error")) {
    msg <- "Could not complete connection to API"
    details <- NULL
    return(list(msg = msg, details = details, data = NULL))
  }
  
  if (httr::http_error(rawdata)) {
    msg <- "Could not complete connection to API"
    details <- httr::status_code(rawdata)
    return(list(msg = msg, details = details, data = NULL))
  }
  
  if (httr::http_type(rawdata) != "application/json") {
    msg <- "API did not return a json object"
    details <- httr::http_type(rawdata)
    return(list(msg = msg, details = details, data = NULL))
  }
  
  rawdata <- rawdata %>% 
    httr::content("text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON(simplifyDataFrame = TRUE)
  
  if (length(rawdata$dataset) == 0) {
    msg <- rawdata$validation$status$name
    details <- rawdata$validation$message
    df <- NULL
  } else {
    if (colname == "H") {
      colnames(rawdata$dataset) <- api_col_names()
    }
    msg <- "Data returned"
    details <- "Connection successful"
    df <- rawdata$dataset
  }
  
  return(list(msg = msg, details = details, data = df))
}