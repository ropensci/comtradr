#' Commodity codes
#'
#' A full dataset of all commodity codes available in the UN Comtrade database.
#'
#' @format `cmd_codes`
#' A dataframe with 8,263 rows and five columns:
#' \describe{
#'  \item{id}{Unique commodity code.}
#'  \item{text}{Description of the commodity.}
#'  \item{parent}{The parent commodity code.}
#'  \item{aggr_level}{The level of aggregation. You can access levels 0, 2, 4, and 6.}
#'  \item{class_code}{The commodity's classification code. We currently only support HS codes.}
#' }
#' @source <https://comtrade.un.org/Data/cache/classificationHS.json>
"cmd_codes"

#' Country codes
#'
#' A full dataset of all reporter and partner codes available in the UN Comtrade database.
#'
#' @format `country_codes`
#' A dataframe with 312 rows and seven columns:
#' \describe{
#'  \item{id}{Unique country code.}
#'  \item{country}{Name of the country (in English).}
#'  \item{iso_3}{The country's ISO 3 code.}
#'  \item{entry_year}{The country's entry into the international system or 1900 (whichever is largest).}
#'  \item{exit_year}{The country's exit from the international system, if applicable.}
#'  \item{reporter}{Indicates whether the country is a reporter in the UN Comtrade database.}
#'  \item{partner}{Indicates whether the country can be reported on by others in the UN Comtrade database. Not all partners are reporters. For example, the World cannot report its trade values.}
#' }
#' @source <https://comtradeapi.un.org/files/v1/app/reference/Reporters.json> and <https://comtradeapi.un.org/files/v1/app/reference/partnerAreas.json>
"country_codes"
