#' UN Comtrade country database query
#'
#' Country names passed to the Comtrade API must have precise
#' spelling/capitalization. This is a helper function for querying the country
#' names/spelling used by Comtrade.. It takes as input a vector of
#' country names, output is any country names that contain any of the input
#' strings, using regex via the base function grepl.
#' For use with the UN Comtrade API, full API docs can be found at
#' \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param search_terms Char vector of country names.
#' @param type str, the country list to use for the search, valid inputs are
#'  "reporter" and "partner".
#' @param ignore.case logical, to be passed along to arg ignore.case within
#'  \code{\link{grepl}}. Default value is TRUE.
#' @param ... additional args to be passed along to \code{\link{grepl}}.
#'
#' @return A character vector of country names that are complete or partial
#'  matches with any of the input country names.
#'
#' @details This function uses regular expressions (regex) to find matches
#'  within the country DB. This means it will treat as a match any country
#'  string that contains the input search term. For more on using regex
#'  within R, see this great tutorial by Gloria Li and Jenny Bryan
#'  \url{http://stat545.com/block022_regular-expression.html}
#'
#' @export
#'
#' @seealso \code{\link{grepl}}
#'
#' @examples
#' # Look up all reporters that contain the terms "korea" and "vietnam"
#' ct_country_lookup(c("korea", "vietnam"), "reporter")
ct_country_lookup <- function(search_terms, type = c("reporter", "partner"),
                              ignore.case = TRUE, ...) {
  # Input validation.
  type <- match.arg(type)
  stopifnot(is.character(search_terms))
  stopifnot(is.logical(ignore.case))

  # If length of search_terms is more than one, transform values into a regex
  # friendly string.
  if (length(search_terms) > 1) {
    search_terms <- paste(search_terms, collapse = "|")
  }

  # Fetch the country databse from ct_env.
  country_df <- get_country_db()

  # Perform the lookup.
  ans <- country_df[country_df[[type]] == TRUE &
                      grepl(search_terms, country_df$country_name,
                            ignore.case = ignore.case, ...),
                    c("country_name")]

  # Check output, if valid then return as is, if empty return "no matches"
  # message.
  if (any(c(length(ans) == 0, is.null(ans), is.na(ans)))) {
    ans <- "No matching results found"
  }

  return(ans)
}
