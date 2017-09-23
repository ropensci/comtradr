#' Get current commodity database type
#'
#' Return the "type" of the current commodity database being used by
#' \code{comtradr}. For a complete list of the different commodity DB
#' types, see "details".
#'
#' @details Below is a list of all of the commodity database "types", with a
#'  very brief description for each. For more information on each of these
#'  types, see
#'  \url{https://comtrade.un.org/data/doc/api/#DataAvailabilityRequests}
#'  \itemize{
#'  \item \code{HS}: Harmonized System (HS), as reported
#'  \item \code{HS1992}: HS 1992
#'  \item \code{HS1996}: HS 1996
#'  \item \code{HS2002}: HS 2002
#'  \item \code{HS2007}: HS 2007
#'  \item \code{HS2012}: HS 2012
#'  \item \code{SITC}: Standard International Trade Classification (SITC), as
#'    reported
#'  \item \code{SITCrev1}: SITC Revision 1
#'  \item \code{SITCrev2}: SITC Revision 2
#'  \item \code{SITCrev3}: SITC Revision 3
#'  \item \code{SITCrev4}: SITC Revision 4
#'  \item \code{BEC}: Broad Economic Categories
#'  \item \code{EB02}: Extended Balance of Payments Services Classification
#'  }
#'
#' @return character vector of the "type" of the current commodity database.
#' @export
#'
#' @examples
#' ct_commodity_db_type()
#'
ct_commodity_db_type <- function() {
  attributes(get_commodity_db())$type
}
