#' @title Interface to the United Nations Comtrade API
#'
#' @description Interface with and extract data from the United Nations
#'   Comtrade API. Comtrade provides country level shipping data for a variety
#'   of commodities, these functions allow for easy API query and data returned
#'   as a tidy data frame.
#'
#' @section Package Vignette:
#'
#' \itemize{
#'   \item \url{../doc/comtradr-vignette.html}
#' }
#'
#' @section Documentation for the Comtrade API:
#'
#' \itemize{
#'   \item Main Comtrade Site \url{https://comtrade.un.org/}
#'   \item Comtrade Data Query Web GUI \url{https://comtrade.un.org/data/}
#'   \item Full API Documentation \url{https://comtrade.un.org/data/doc/api/}
#' }
#'
#' @section Development links:
#'
#' \itemize{
#'   \item \url{https://github.com/ChrisMuir/comtradr}
#'   \item Report bugs at \url{https://github.com/ChrisMuir/comtradr/issues}
#' }
#'
#' @section \code{comtradr} features the following functions:
#' \itemize{
#'   \item \code{\link{ct_commodity_db_type}}
#'   \item \code{\link{ct_commodity_lookup}}
#'   \item \code{\link{ct_country_lookup}}
#'   \item \code{\link{ct_get_remaining_hourly_queries}}
#'   \item \code{\link{ct_get_reset_time}}
#'   \item \code{\link{ct_register_token}}
#'   \item \code{\link{ct_search}}
#'   \item \code{\link{ct_update_databases}}
#'   \item \code{\link{ct_use_pretty_cols}}
#' }
#'
#' @importFrom magrittr "%>%"
#' @docType package
#' @name comtradr
NULL

#' "pretty" column headers for Comtrade API data.
#'
#' Named vector of polished column headers, intended for use with plots,
#' publication tables, etc.
#'
#' @docType data
#' @name ct_pretty_cols
#' @format Named vector, with the polished column headers as the names, and
#'  the machine-readable column headers as the values. Each element is meant
#'  to be treated as a key-value pair. The function \code{\link{ct_search}}
#'  returns data with the machine-readable column headers by default.
#' @examples
#' data(ct_pretty_cols)
NULL
