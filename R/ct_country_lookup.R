#' UN Comtrade country database query
#'
#' Country names passed to the Comtrade API must have precise
#' spelling/capitalization. This is a helper function for querying the country
#' names/spelling used by Comtrade.. It takes as input a vector of
#' country names, output is any country names that contain any of the input
#' strings, using regex via the base function grepl (search is case insensitive).
#' For use with the UN Comtrade API, full API docs can be found at
#' \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param loc Char vector of country names.
#' @param type The country list to use for the search, valid inputs are
#'  "reporter" and "partner".
#'
#' @return A character vector of country names that are complete or partial
#'  matches with any of the input country names.
#' @export
#'
#' @examples
#' # Look up all reporters that contain the terms "korea" and "vietnam"
#' ct_country_lookup(c("korea", "vietnam"), "reporter")
ct_country_lookup <- function(loc, type = c("reporter", "partner")) {
  # Input validation.
  type <- match.arg(type)
  if (!is.character(loc) || length(loc) < 1) {
    stop("param 'loc' must be a char vector with length greater than zero",
         call. = FALSE)
  }

  # If length of loc is more than one, transform values into a regex friendly
  # string.
  if (length(loc) > 1) {
    loc <- paste(loc, collapse = "|")
  }

  # Fetch the country databse from ct_env.
  country_df <- get("country_df", envir = ct_env)

  # Perform the lookup.
  ans <- country_df[country_df$type == type &
                      grepl(loc, country_df$`country name`,
                            ignore.case = TRUE),
                    c("country name")]

  # Check output, if valid then return as is, if empty return "no matches"
  # message.
  if (length(ans) == 0 || is.null(ans) || is.na(ans)) {
    ans <- "No matching results found"
  }

  return(ans)
}
