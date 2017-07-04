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
#' commodity_lookup("halibut",
#'                  commoditydf,
#'                  return_code = FALSE,
#'                  return_char = FALSE,
#'                  verbose = TRUE)
#' $halibut
#' [1] "030221 - Fish; halibut (reinhardtius hippoglossoides, hippoglossus
#'  hippoglossus, hippoglossus stenolepis), fresh or chilled (excluding
#'  fillets, livers, roes and other fish meat of heading no. 0304)"
#' [2] "030331 - Fish; halibut (reinhardtius hippoglossoides, hippoglossus
#'  hippoglossus, hippoglossus stenolepis), frozen (excluding fillets, livers,
#'  roes and other fish meat of heading no. 0304)"
#'
#' # Look up commodity codes related to "shrimp".
#' comtradr::commodity_lookup("shrimp",
#'                            commoditydf,
#'                            return_code = TRUE,
#'                            return_char = FALSE,
#'                            verbose = TRUE)
#' $shrimp
#' [1] "030613" "030616" "030617" "030623" "030626" "030627" "030635" "030636"
#'  "030695" "160520" "160521" "160529" "160540"
#' }

commodity_lookup <- function(values, lookuptable, return_code = FALSE,
                             return_char = FALSE, verbose = TRUE) {
  stopifnot(mode(values) %in% c("numeric", "character"))
  stopifnot(is.data.frame(lookuptable))
  values <- as.character(values)

  # For each element of input param "values", fetch all commodity descriptions
  # and/or codes from the lookuptable.
  ans <- sapply(values, function(x) {
    # Determine whether the param 'value' is a commodity or a code, then
    # perform the look up.
    if (grepl("[a-z]", x)) {
      if (return_code) {
        # For char input (will do commodity lookup), and will return the HS
        # code for each returned value.
        lookuptable[grepl(x, lookuptable$commodity, ignore.case = TRUE),
                    c("code")]
      } else {
        # For char input (will do commodity lookup), and will return the full
        # commodity description for each returned value.
        lookuptable[grepl(x, lookuptable$commodity, ignore.case = TRUE),
                    c("commodity")]
      }
    } else {
      if (return_code) {
        # For numeric input (will do HS code lookup), and will return the HS
        # code for each returned value.
        lookuptable[grepl(x, lookuptable$code, ignore.case = TRUE),
                    c("code")]
      } else {
        # For numeric input (will do HS code lookup), and will return the HS
        # code for each returned value.
        lookuptable[grepl(x, lookuptable$code, ignore.case = TRUE),
                    c("commodity")]
      }
    }
  })

  # If ans is a matrix, convert to a char vector if return_char is TRUE,
  # otherwise convert to a list.
  if (is.matrix(ans)) {
    if (return_char) {
      ans <- as.vector(ans)
    } else {
      ans <- ans %>%
        as.vector %>%
        list %>%
        `names<-`(values)
    }
  }

  # If ans is a list, check the legnth of each element. If any have length
  # zero, create a warning message that will be printed to console if input
  # param "verbose" is TRUE. If there are no elements with length zero,
  # convert ans to a char vector if return_char is TRUE.
  if (is.list(ans)) {
    if (verbose) {
      check_len <- vapply(ans, length, integer(1), USE.NAMES = FALSE)
      if (any(check_len == 0)) {
        if (any(check_len > 0)) {
          msg <- paste0("There were no matching results found for inputs: ",
                        paste(values[which(check_len == 0)], collapse = ", "))
        } else {
          msg <- "There were no matching results found"
        }
      }
    }
    if (return_char) {
      ans <- ans %>%
        unname %>%
        unlist
    }
  }

  # If ans is a char vector, convert to a list if return_char is FALSE, otherwise
  # unname the char vector.
  if (mode(ans) == "character") {
    if (return_char) {
      ans <- unname(ans)
    } else{
      ans <- lapply(ans, function(x) x)
    }
  }

  if (exists("msg")) {
    warning(msg, call. = FALSE)
  }
  return(ans)
}
