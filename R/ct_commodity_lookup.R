#' UN Comtrade commodities database query
#'
#' The Comtrade API requires that searches for specific commodities be done
#' using commodity codes. This is a helper function for querying the
#' Comtrade commodity database. It takes as input a vector of
#' commodities or commodity codes. Output is a list or vector of commodity
#' descriptions or codes associated with the input search_terms. For use with
#' the UN Comtrade API, full API docs can be found at
#' \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param search_terms Commodity names or commodity codes, as a char or numeric
#'  vector.
#' @param return_code Logical, if set to FALSE, the function will return a
#'  set of commodity descriptions along with commodity codes (as a single
#'  string for each match found), if set to TRUE it will return only the
#'  commodity codes. Default value is FALSE.
#' @param return_char Logical, if set to FALSE, the function will return the
#'  matches as a named list, if set to TRUE it will return them as a character
#'  vector. Default value is FALSE.
#' @param verbose Logical, if set to TRUE, a warning message will print to
#'  console if any of the elements of input "search_terms" returned no matches
#'  (message will indicate which elements returned no data). Default is TRUE.
#'
#' @return A list or character vector of commodity descriptions and/or
#'  commodity codes that are matches with the elements of "search_terms".
#'
#' @details This function uses regular expressions (regex) to find matches
#'  within the commodity DB. This means it will treat as a match any commodity
#'  description that contains the input search term. For more on using regex
#'  within R, see this great tutorial by Gloria Li and Jenny Bryan
#'  \url{http://stat545.com/block022_regular-expression.html}
#'
#' @export
#'
#' @seealso \code{\link{grepl}}
#'
#' @examples
#' # Look up commodity descriptions related to "halibut"
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

ct_commodity_lookup <- function(search_terms, return_code = FALSE,
                                return_char = FALSE, verbose = TRUE) {
  stopifnot(mode(search_terms) %in% c("numeric", "character"))
  search_terms <- as.character(search_terms)

  # Fetch the commodity database from ct_env.
  commodity_df <- get_commodity_db()


  # transform input arg "return_code" to match the col name indicated
  # (TRUE == "code", FALSE == "commodity").
  if (return_code) {
    return_col <- "code"
  } else {
    return_col <- "commodity"
  }

  # For each element of input arg "search_terms", fetch all commodity
  # descriptions and/or codes from the database. Output will be a list.
  ans <- purrr::map(search_terms, function(x) {
    # Determine whether the param 'value' is a commodity or a code, then
    # perform the look up.
    if (grepl("[a-z]", x)) {
      lu <- "commodity"
    } else {
      lu <- "code"
    }
    commodity_df[grepl(x, commodity_df[[lu]], ignore.case = TRUE), return_col]
  })

  # If "verbose" == TRUE, create warning message if any of the elements of
  # input arg "search_terms" produced no search results.
  if (verbose) {
    check_len <- purrr::map_int(ans, length)
    if (any(check_len == 0)) {
      if (any(check_len > 0)) {
        msg <- paste0(
          "There were no matching results found for inputs: ",
          paste(search_terms[which(check_len == 0)], collapse = ", ")
        )
      } else {
        msg <- "There were no matching results found"
      }
      warning(msg, call. = FALSE)
    }
  }

  # If "return_char" == TRUE, unlist obj "ans". Otherwise, assign names to the
  # elements of obj "ans" (names will be taken from input arg "search_terms").
  if (return_char) {
    ans <- unlist(ans, FALSE, FALSE)
  } else {
    names(ans) <- search_terms
  }
  return(ans)
}
