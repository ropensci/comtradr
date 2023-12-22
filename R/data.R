#' Country codes
#'
#' A full dataset of all reporter and partner codes available in the UN Comtrade database.
#'
#' @format `country_codes`
#' A dataframe with 312 rows and eight columns:
#' \describe{
#'  \item{id}{Unique country code.}
#'  \item{country}{Name of the country (in English).}
#'  \item{iso_3}{The country's ISO 3 code.}
#'  \item{entry_year}{The country's entry into the international system or 1900 (whichever is largest).}
#'  \item{exit_year}{The country's exit from the international system, if applicable.}
#'  \item{group}{Indicates whether the entity is a group of countries. For example, ASEAN or the European Union.}
#'  \item{reporter}{Indicates whether the country is a reporter in the UN Comtrade database.}
#'  \item{partner}{Indicates whether the country can be reported on by others in the UN Comtrade database. Not all partners are reporters. For example, the World cannot report its trade values.}
#' }
#' @source <https://comtradeapi.un.org/files/v1/app/reference/Reporters.json> and <https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json>
"country_codes"

#' ct_pretty_cols
#'
#' A data.frame with a matched list of tidy and untidy column names for the results.
#'
#' @format `country_codes`
#' A dataframe with 47 rows and twi columns:
#' \describe{
#'  \item{to}{tidy columns}
#'  \item{from}{original column names}
#' }
"ct_pretty_cols"
