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
#'  site changes from \url{https://comtrade.un.org/data/cache/}. This partial
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
  commodity_type <- match.arg(commodity_type)

  if (!is.null(commodity_url)) {
    commodity_url <- commodity_url
  } else {
    commodity_url <- "https://comtrade.un.org/data/cache/"
  }

  if (!is.null(reporter_url)) {
    reporter_url <- reporter_url
  } else {
    reporter_url <- "https://comtrade.un.org/data/cache/reporterAreas.json"
  }

  if (!is.null(partner_url)) {
    partner_url <- partner_url
  } else {
    partner_url <- "https://comtrade.un.org/data/cache/partnerAreas.json"
  }

  # Append the correct url str to the end of commodity_url, based on arg
  # "commodity_type".
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

  # If "force" is FALSE, get the current databases from ct_env.
  if (!force) {
    country_df <- get("country_df", envir = ct_env)
    commodity_df <- get("commodity_df", envir = ct_env)
  }

  # Get the commodity database from the Comtrade website. Compare the
  # "last-modified" date value within the header to the value in variable
  # "date" within the current commodity database. If the "last-modified"
  # date is newer than the date within the current database, or the value
  # passed to arg "commodty_type" doesn't match the values in variable "type"
  # of the current database, the old DB will be replaced by the newer DB.
  # Replacement will be for both the current session and within the data dir
  # of the comtradr package.
  res <- httr::GET(commodity_url)
  if (force ||
      commodity_type != commodity_df$type[1] ||
      httr::headers(res)$`last-modified` > commodity_df$date[1]) {
    # Extract data frame.
    df <- res %>%
      httr::content("text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      magrittr::extract2("results") %>%
      `colnames<-`(c("code", "commodity", "parent"))
    df$type <- commodity_type
    df$date <- curr_date
    # Save df to data dir of the comtradr package.
    save(
      df,
      file = paste0(system.file("extdata", package = "comtradr"),
                    "/commodity_table.rda"),
      compress = "bzip2"
    )
    # Save df to ct_env.
    assign("commmodity_df", df, envir = ct_env)
    # Update the output message.
    if (verbose) {
      msg <- paste0("Updates found. The following datasets have been ",
                    "downloaded: commodities DB")
    }
  }

  # Get the reporter country database from the Comtrade website. Compare the
  # "last-modified" date value within the header to the value in variable
  # "date" for reporter countries within the current country databse. If the
  # "last-modified" date is newer than the date within the current country
  # DB, the reporter countries portion of the old DB will be replaced by
  # the newer DB, both for the current session and within the data dir of
  # the comtradr package.
  country_update <- FALSE
  res <- httr::GET(reporter_url)
  if (force ||
      httr::headers(res)$`last-modified` >
      country_df[country_df$type == "reporter", ]$date[1]) {
    # Extract data frame.
    df <- res %>%
      httr::content("text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      magrittr::extract2("results") %>%
      `colnames<-`(c("code", "country name"))
    df$type <- "reporter"
    df$date <- curr_date
    # Replace the reporters portion of the current country_df with the new
    # data.
    if (!force) {
      country_update <- TRUE
      country_df[country_df$type == "reporter", ] <- df
    } else {
      country_df <- df
    }
    # Update the output message.
    if (verbose) {
      if (grepl("Updates found", msg, fixed = TRUE)) {
        msg <- paste0(msg, ", reporter countries")
      } else {
        msg <- paste0("Updates found. The following datasets have been ",
                      "downloaded: reporter countries")
      }
    }
  }

  # Get the partner country database from the Comtrade website. Compare the
  # "last-modified" date value within the header to the value in variable
  # "date" for partner countries within the current country database. If the
  # "last-modified" date is newer than the date within the current country
  # DB, the partner countries portion of the old DB will be replaced by
  # the newer DB, both for the current session and within the data dir of
  # the comtradr package.
  res <- httr::GET(partner_url)
  if (force ||
      httr::headers(res)$`last-modified` >
      country_df[country_df$type == "partner", ]$date[1]) {
    # Extract data frame.
    df <- res %>%
      httr::content("text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      magrittr::extract2("results") %>%
      `colnames<-`(c("code", "country name"))
    df$type <- "partner"
    df$date <- curr_date
    # Replace the reporters portion of the current country_df with the new
    # data.
    if (!force) {
      if (!country_update) {
        country_update <- TRUE
      }
      country_df[country_df$type == "partner", ] <- df
    } else {
      country_df <- rbind(country_df, df)
    }
    # Update the output message.
    if (verbose) {
      if (grepl("Updates found", msg, fixed = TRUE)) {
        msg <- paste0(msg, ", partner countries")
      } else {
        msg <- paste0("Updates found. The following datasets have been ",
                      "downloaded: partner countries")
      }
    }
  }

  # If updates were made to either the reporter OR partner portions of the
  # country database, then save the updated country DB to the data dir of the
  # comtradr package, and update "country_df" within ct_env.
  save(
    country_df,
    file = paste0(system.file("extdata", package = "comtradr"),
                  "/country_table.rda"),
    compress = "bzip2"
  )
  # Save country_df to ct_env.
  assign("country_df", country_df, envir = ct_env)

  # Finally, print to console the results of the update function (msg).
  if (verbose) {
    message(msg)
  }
}
