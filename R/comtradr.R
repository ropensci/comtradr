#' @title Interface to the United Nations Comtrade API
#'
#' @description Interface with and extract data from the United Nations
#'   Comtrade API. Comtrade provides country level shipping data for a variety
#'   of commodities, these functions allow for easy API query and data returned
#'   as a tidy data frame.
#'
#' @section Documentation for the Comtrade API:
#'
#' \itemize{
#'   \item Main Comtrade Site \url{https://comtrade.un.org/}
#'   \item Comtrade Data Query Web GUI \url{https://comtrade.un.org/data/}
#'   \item Full API Documentation \url{https://comtrade.un.org/data/doc/api/}
#' }
#'
#' @section \code{comtradr} features the following functions:
#' \itemize{
#'   \item \code{\link{ct_search}}
#'   \item \code{\link{ct_countries_table}}
#'   \item \code{\link{ct_commodities_table}}
#'   \item \code{\link{ct_country_lookup}}
#'   \item \code{\link{ct_commodity_lookup}}
#' }
#'
#' @docType package
#' @name comtradr
NULL
