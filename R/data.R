#' Commodity codes
#'
#' A full dataset of all commodity codes available in the UN Comtrade database.
#'
#' @format `cmd_codes`
#' A dataframe with 8,267 rows and four columns:
#' \describe{
#'  \item{id}{Unique commodity code.}
#'  \item{text}{Description of the commodity.}
#'  \item{parent}{The parent commodity code.}
#'  \item{class_code}{The commodity's classification code. We currently only support HS codes.}
#' }
#' @source <https://comtrade.un.org/Data/cache/classificationHS.json>
"cmd_codes"
