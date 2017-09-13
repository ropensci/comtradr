#' UN Comtrade reporter/partner lookup table query
#'
#' Country names passed to the Comtrade API must have precise
#' spelling/capitalization. This is a helper function for querying the country
#' code lookup table that's created by function
#' \code{\link{ct_countries_table}}. It takes as input a vector of
#' country names, output is any country names that contain any of the input
#' strings, using regex via the base function grepl (search is case insensitive).
#' For use with the UN Comtrade API, full API docs can be found at
#' \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param loc Char vector of country names.
#' @param type The country list to use for the search, valid inputs are
#'  "reporter" and "partner".
#' @param lookuptable Dataframe of country names and codes (intended input is
#'  the dataframe created by function \code{\link{ct_countries_table}}).
#'
#' @return A character vector of country names that are complete or partial
#'  matches with any of the input country names.
#' @export
#'
#' @examples \dontrun{
#' # Look up all reporters that contain the terms "korea" and "vietnam"
#' countrydf <- ct_countries_table()
#' ct_country_lookup(c("korea", "vietnam"), "reporter", countrydf)
#' }
ct_country_lookup <- function(loc, type = c("reporter", "partner"),
                              lookuptable) {

  type <- match.arg(type)

  if (!is.character(loc) || length(loc) < 1) {
    stop("param 'loc' must be a char vector with length greater than zero",
         call. = FALSE)
  }

  if (!is.data.frame(lookuptable)) {
    stop("param 'lookuptable' must be a dataframe", call. = FALSE)
  }

  if (length(loc) > 1) {
    loc <- paste(loc, collapse = "|")
  }

  ans <- lookuptable[lookuptable$type == type &
                       grepl(loc, lookuptable$`country name`,
                             ignore.case = TRUE), c("country name")]

  # Check output, if valid then return as is, if empty return "no matches"
  # message.
  if (length(ans) == 0 || is.null(ans) || is.na(ans)) {
    ans <- "No matching results found"
  }

  return(ans)
}
