#' UN Comtrade commodities lookup table query
#'
#' The Comtrade API requires that searches for specific commodities be done
#' using commodity codes. This is a helper function for querying the
#' commodity code lookup table that's created by function
#' \code{\link{ct_commodities_table}}. It takes as input a vector of
#' commodities or commodity codes. If input is a commodity, then output is all
#' commodity codes and descriptions that are associated with the input value.
#' If input is a commodity code, then output is the commodity description
#' associated with that input code. For use with the UN Comtrade API, full API
#' docs can be found at \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param values Commodity names or commodity codes, as a char or numeric
#'  vector.
#' @param lookuptable Dataframe of commodity descriptions and codes (intended
#'  input is the dataframe created by function
#'  \code{\link{ct_commodities_table}}).
#' @param return_code Logical, if set to FALSE, the function will return a
#'  set of commodity descriptions along with commodity codes (as a single
#'  string for each match found), if set to TRUE it will return only the
#'  commodity codes. Default value is FALSE.
#' @param return_char Logical, if set to FALSE, the function will return the
#'  matches as a named list, if set to TRUE it will return them as a character
#'  vector. Default value is FALSE.
#' @param verbose Logical, if set to TRUE, a warning message will print to
#'  console if any of the elements of input "values" returned no matches
#'  (message will indicate which elements returned no data). Default is TRUE.
#'
#' @return A list or character vector of commodity descriptions and/or
#'  commodity codes that are matches with the elements of "values".
#' @export
#'
#' @examples \dontrun{
#' # Look up commodity descriptions related to "halibut"
#' commoditydf <- ct_commodities_table("HS")
#' ct_commodity_lookup("halibut",
#'                     commoditydf,
#'                     return_code = FALSE,
#'                     return_char = FALSE,
#'                     verbose = TRUE)
#'
#' # Look up commodity codes related to "shrimp".
#' ct_commodity_lookup("shrimp",
#'                     commoditydf,
#'                     return_code = TRUE,
#'                     return_char = FALSE,
#'                     verbose = TRUE)
#' }

ct_commodity_lookup <- function(values, lookuptable, return_code = FALSE,
                                return_char = FALSE, verbose = TRUE) {
  stopifnot(mode(values) %in% c("numeric", "character"))
  stopifnot(is.data.frame(lookuptable))
  values <- as.character(values)

  # transform input arg "return_code" to match the col name indicated
  # (TRUE == "code", FALSE == "commodity").
  if (return_code) {
    return_col <- "code"
  } else {
    return_col <- "commodity"
  }

  # For each element of input param "values", fetch all commodity descriptions
  # and/or codes from the lookuptable. Output will be a list.
  ans <- purrr::map(values, function(x) {
    # Determine whether the param 'value' is a commodity or a code, then
    # perform the look up.
    if (grepl("[a-z]", x)) {
      lu_col <- "commodity"
    } else {
      lu_col <- "code"
    }
    lookuptable[grepl(x, lookuptable[[lu_col]], ignore.case = TRUE), return_col]
  })

  # If "verbose" == TRUE, create warning message if any of the elements of input
  # arg "values" produced no search results.
  if (verbose) {
    check_len <- purrr::map_int(ans, length)
    if (any(check_len == 0)) {
      if (any(check_len > 0)) {
        msg <- paste0("There were no matching results found for inputs: ",
                      paste(values[which(check_len == 0)], collapse = ", "))
      } else {
        msg <- "There were no matching results found"
      }
      warning(msg, call. = FALSE)
    }
  }

  # If "return_char" == TRUE, unlist obj "ans". Otherwise, assign names to the
  # elements of obj "ans" (names will be taken from input arg "values").
  if (return_char) {
    ans <- unlist(ans, FALSE, FALSE)
  } else {
    names(ans) <- values
  }
  return(ans)
}
