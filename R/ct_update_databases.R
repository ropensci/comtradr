#' Check for updates to country/commodity databases
#'
#' Use of the Comtrade API requires access to the Comtrade countries database
#' and commodities database. The \code{comtradr} package keeps each DB saved
#' as a data frame in the package directory, as Comtrade makes updates to
#' these DB's infrequently (roughly once per year).
#'
#' This function will check to see if Comtrade has made any updates to either
#' database. If an update is found, it will download the updated DB and save
#' it to the \code{comtradr} package directory, and update the DB for use
#' within the current R session.
#'
#' @param force logical, if TRUE, both the country and commodity databases
#'  will be downloaded, regardless of the status of the DB's on file. Default
#'  value is FALSE.
#' @param verbose logical, if TRUE, an update status message will be printed
#'  to console. Default value is TRUE.
#' @param commodity_type Trade data classification scheme to use, see
#'  "details" for a list of the valid inputs. Default value is "HS", which is
#'  the default "type" of the commodity database on file upon install of
#'  \code{comtradr}. Please note that if the value passed to this arg doesn't
#'  match the values in variable "type" of the current commodity DB, then
#'  this function will replace the current commodity DB with that of the
#'  type specified by this arg. If you don't intend to change the type of the
#'  current commodity DB, then no input for this arg is required. To see
#'  the "type" of the current commodity DB, use
#'  \code{\link{ct_commodity_db_type}}.
#' @param commodity_url Default value NULL, otherwise this should be the base
#'  url of the Comtrade json data directory. Only necessary if the Comtrade
#'  site changes from "https://comtrade.un.org/data/cache/". This partial
#'  url string will have a commodity extension appended to it to create a
#'  valid url. The commodity extension will be chosen based on the input to
#'  arg \code{commodity_type}.
#' @param reporter_url Default value NULL, otherwise this should be a url as a
#'  char string that points to the reporter areas JSON dataset on the Comtrade
#'  website. Only necessary if the Comtrade site changes from
#'  \url{https://comtrade.un.org/data/cache/reporterAreas.json}
#' @param partner_url Default value NULL, otherwise this should be a url as a
#'  char string that points to the reporter areas JSON dataset on the Comtrade
#'  website. Only necessary if the Comtrade site changes from
#'  \url{https://comtrade.un.org/data/cache/partnerAreas.json}
#'
#' @details The default for arg \code{commodity_type} is \code{HS}. Below is a
#'  list of all valid inputs with a very brief description for each, for more
#'  information on each of these types, see
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
#' @return Updated database of commodities and countries.
#'
#' @export
#'
#' @examples \dontrun{
#' ct_update_databases()
#' }
ct_update_databases <- function(force = FALSE, verbose = TRUE,
                                commodity_type = c("HS", "HS1992", "HS1996",
                                                   "HS2002", "HS2007",
                                                   "HS2012", "SITC",
                                                   "SITCrev1", "SITCrev2",
                                                   "SITCrev3", "SITCrev4",
                                                   "BEC", "EB02"),
                                commodity_url = NULL,
                                reporter_url = NULL,
                                partner_url = NULL) {

  # Input validation.
  stopifnot(is.logical(force))
  stopifnot(is.logical(verbose))
  if (!is.null(commodity_url)) {
    stopifnot(is.character(commodity_url))
    commodity_url <- commodity_url
  } else {
    commodity_url <- "https://comtrade.un.org/data/cache/"
  }

  if (!is.null(reporter_url)) {
    stopifnot(is.character(reporter_url))
    reporter_url <- reporter_url
  } else {
    reporter_url <- "https://comtrade.un.org/data/cache/reporterAreas.json"
  }

  if (!is.null(partner_url)) {
    stopifnot(is.character(partner_url))
    partner_url <- partner_url
  } else {
    partner_url <- "https://comtrade.un.org/data/cache/partnerAreas.json"
  }

  # Append the correct url str to the end of commodity_url, based on arg
  # "commodity_type".
  commodity_type <- match.arg(commodity_type)
  if (commodity_type == "HS") {
    commodity_url <- paste0(commodity_url, "classificationHS.json")
  } else if (commodity_type == "HS1992") {
    commodity_url <- paste0(commodity_url, "classificationH0.json")
  } else if (commodity_type == "HS1996") {
    commodity_url <- paste0(commodity_url, "classificationH1.json")
  } else if (commodity_type == "HS2002") {
    commodity_url <- paste0(commodity_url, "classificationH2.json")
  } else if (commodity_type == "HS2007") {
    commodity_url <- paste0(commodity_url, "classificationH3.json")
  } else if (commodity_type == "HS2012") {
    commodity_url <- paste0(commodity_url, "classificationH4.json")
  } else if (commodity_type == "SITC") {
    commodity_url <- paste0(commodity_url, "classificationST.json")
  } else if (commodity_type == "SITCrev1") {
    commodity_url <- paste0(commodity_url, "classificationS1.json")
  } else if (commodity_type == "SITCrev2") {
    commodity_url <- paste0(commodity_url, "classificationS2.json")
  } else if (commodity_type == "SITCrev3") {
    commodity_url <- paste0(commodity_url, "classificationS3.json")
  } else if (commodity_type == "SITCrev4") {
    commodity_url <- paste0(commodity_url, "classificationS4.json")
  } else if (commodity_type == "BEC") {
    commodity_url <- paste0(commodity_url, "classificationBEC.json")
  } else if (commodity_type == "EB02") {
    commodity_url <- paste0(commodity_url, "classificationEB02.json")
  }

  # Create output message.
  if (verbose) {
    msg <- "All DB's are up to date, no action required"
  }

  # Get the current date/time.
  curr_date <- format(Sys.time(), "%a, %d %b %Y %X %Z")

  # If "force" is FALSE, get the current databases as data frames.
  if (!force) {
    country_df <- get_country_db()
    commodity_df <- get_commodity_db()
  }

  # Get the commodity database from the Comtrade website. Compare the
  # "last-modified" date value within the header to the "date" attribute of
  # the current commodity database on file. If the "last-modified" date is
  # newer than the date of the current database, or the value passed to arg
  # "commodty_type" doesn't match the "type" attribute of the current database,
  # or arg "force" is TRUE, the old DB will be replaced by the newer DB.
  # Replacement will be for both the current session and within the data dir
  # of the comtradr package.
  res <- httr::GET(commodity_url, httr::user_agent(get("ua", envir = ct_env)))
  if (force ||
      commodity_type != attributes(commodity_df)$type ||
      httr::headers(res)$`last-modified` > attributes(commodity_df)$date) {
    # Extract data frame.
    commodity_df <- res %>%
      httr::content("text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      magrittr::extract2("results") %>%
      `colnames<-`(c("code", "commodity", "parent"))
    # Assign attributes to the data frame (current date/time, and type).
    attributes(commodity_df)$date <- curr_date
    attributes(commodity_df)$type <- commodity_type
    # Save df to data dir of the comtradr package.
    save(
      commodity_df,
      file = paste0(system.file("extdata", package = "comtradr"),
                    "/commodity_table.rda"),
      compress = "bzip2"
    )
    # Save df to ct_env.
    assign("commodity_df", commodity_df, envir = ct_env)
    # Update the output message.
    if (verbose) {
      msg <- paste0("Updates found. The following datasets have been ",
                    "downloaded: commodities DB")
    }
  }

  # Get the reporter country database and the partner country database from
  # the Comtrade website. Compare the "last-modified" date value within the
  # header of each to the "date" attribute of the current country_table
  # database on file. If either of the "last-modified" dates is newer than the
  # date of the database on file, or if arg "force" is TRUE, replace the
  # database on file with the data pulled from the Comtrade website.
  country_update <- FALSE
  res_rep <- httr::GET(reporter_url,
                       httr::user_agent(get("ua", envir = ct_env)))
  res_par <- httr::GET(partner_url,
                       httr::user_agent(get("ua", envir = ct_env)))
  if (force ||
      httr::headers(res_rep)$`last-modified` > attributes(country_df)$date ||
      httr::headers(res_rep)$`last-modified` > attributes(country_df)$date) {
    country_update <- TRUE
    # Get reporters dataset as data frame.
    reporters <- res_rep %>%
      httr::content("text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      magrittr::extract2("results") %>%
      `colnames<-`(c("code", "country_name"))
    # Get partners dataset as data frame.
    partners <- res_par %>%
      httr::content("text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      magrittr::extract2("results") %>%
      `colnames<-`(c("code", "country_name"))
    # Get all countries and codes as vectors.
    countries <- c(reporters$country_name, partners$country_name)
    codes <- c(reporters$code, partners$code)
    # Initialize output data frame.
    country_df <- data.frame("country_name" = unique(countries),
                             stringsAsFactors = FALSE)
    # Add country codes to country_df.
    country_df$code <- vapply(unique(countries), function(x) {
      codes[match(x, countries)]
    }, character(1), USE.NAMES = FALSE)
    # Add logical vector indicating, for each obs of country_df, whether the
    # country appears as a reporter.
    country_df$reporter <- vapply(unique(countries), function(x) {
      any(reporters$country_name == x)
    }, logical(1), USE.NAMES = FALSE)
    # Add logical vector indicating, for each obs of country_df, whether the
    # country appears as a partner.
    country_df$partner <- vapply(unique(countries), function(x) {
      any(partners$country_name == x)
    }, logical(1), USE.NAMES = FALSE)
  }
  # Assign attributes to the data frame (current date/time).
  attributes(country_df)$date <- curr_date

  # If updates were found for the country reference dataset, then save the
  # updated country DB to the data dir of the comtradr package, and update
  # "country_df" within ct_env.
  if (country_update) {
    save(
      country_df,
      file = paste0(system.file("extdata", package = "comtradr"),
                    "/country_table.rda"),
      compress = "bzip2"
    )
    # Save country_df to ct_env.
    assign("country_df", country_df, envir = ct_env)
    # Update the output message.
    if (verbose) {
      if (grepl("Updates found", msg, fixed = TRUE)) {
        msg <- paste0(msg, ", countries DB")
      } else {
        msg <- paste0("Updates found. The following datasets have been ",
                      "downloaded: countries DB")
      }
    }
  }

  # Finally, print to console the results of the update function (msg).
  if (verbose) {
    message(msg)
  }
}
