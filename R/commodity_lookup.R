#' UN Comtrade commodities lookup table query
#' 
#' The Comtrade API requires that searches for specific commodities be done 
#' using commodity codes. This is a helper function for querying the 
#' commodity code lookup table that's created by function 
#' \code{\link{ct_commodities_table}}. It takes as input a commodity or 
#' commodity code as a vector. If input is a commodity, then output is all 
#' commodity codes and descriptions that are associated with the input value. 
#' If input is a commodity code, then output is the commodity description 
#' associated with that input code. For use with the UN Comtrade API, full API 
#' docs can be found at \url{https://comtrade.un.org/data/doc/api/}
#'
#' @param value Commodity name or commodity code, as a char or numeric vector.
#' @param lookuptable Dataframe of commodity descriptions and codes (intended 
#'  input is the dataframe created by function 
#'  \code{\link{ct_commodities_table}}).
#'
#' @return A character vector of commodity descriptions that are matches with 
#'  the input.
#' @export
#'
#' @examples \dontrun{
#' # Look up commodity codes related to "halibut"
#' commoditydf <- ct_commodities_table("HS")
#' commodity_lookup("halibut", commoditydf)
#' [1] "030221 - Halibut, fresh or chilled, whole"
#' [2] "030229 - Flatfish, fresh/chilled not halibut/plaice/sole, whol"
#' [3] "030331 - Halibut, frozen, whole"
#' [4] "030339 - Flatfish except halibut, plaice or sole, frozen, whol"
#' }
commodity_lookup <- function(value, lookuptable) {
  
  value <- as.character(value)
  
  if (length(value) != 1) {
    stop("param 'value' must be of length 1", call. = FALSE)
  }
  
  if (!is.data.frame(lookuptable)) {
    stop("param 'lookuptable' must be a dataframe", call. = FALSE)
  }
  
  # Determine whether the param 'value' is a commodity or a code, then perform 
  # the look up.
  if (grepl("[a-z]", value)) {
    # Commodity lookup
    ans <- lookuptable[grepl(value, lookuptable$commodity, ignore.case = TRUE), 
                       c("commodity")]
  } else {
    # Code lookup
    ans <- lookuptable[grepl(value, lookuptable$code, ignore.case = TRUE), 
                       c("commodity")]
  }
  
  # Check output, if valid then return as is, if empty return "no matches" 
  # message.
  if (length(ans) == 0 || is.null(ans) || is.na(ans)) {
    ans <- "No matching results found"
  }
  
  return(ans)
}