#' check_params
#'
#' Checks that input parameters are valid and in compliance with the comtrade API.
#'
#' @param frequency The frequency of returned trade data, default is 'A' for annual. Alternative is 'M' for monthly. The default value is 'A'.
#' @param commodity_classification The used classification scheme for the commodity code. As of now, only HS codes are supported, so default is 'HS'.
#' @param commodity_code The commodity code that you would like to investigate. The default value is TOTAL, implying the sum of all commodities. Multiple values can be supplied as a character vector.
#' @param flow_direction The direction of flows, e.g. whether you would like to get data on reported imports or exports. Possible values are "import" for imports, "export" for exports. Multiple values can be supplied as a character vector. The default value is 'all' for imports, exports, re-imports and re-exports.
#' @param reporter This has to be a vector of character values specifying one or multiple reporter countries in the iso3c format. The reporter is the country that supplied the data to the UN. The string 'all' can be supplied to return values for all reporter countries that are not labelled as 'group' by the UN (e.g. ASEAN countries)
#' @param partner This has to be a vector of character values specifying the partner country in the iso3c format. The partner area is the country with whom the reporter has reported trade relations. The string 'all' can be supplied to return values for all partner countries that are not labelled as 'group' by the UN (e.g. ASEAN countries or the entire World). The value 'world' can be supplied, to include trade with all partner countries aggregated globally.
#' @param start_date Start date of a time period.
#' @param end_date End date of a time period.
#' @param verbose whether the function sends status updates to the console
#' @param mode_of_transport The Mode of Transport is set to `0`, which is the default for TOTAL across all modes of transportation. This parameter is so far not being validated.
#' @param partner_2 This value is set as a default to `0`, which is most likely the most general value and also the default on the Comtrade website.
#' @param customs_code The customs code is set to the default of `C00` which is the default for TOTAL across all customs procedures.
#' @param ... You can pass in further parameters to the API that will not be checked and passed on as query parameters exactly as they are put in.
#'
#' @return returns a list of named parameters for building a request
check_params <- function(frequency = 'A',
                         commodity_classification = 'HS',
                         commodity_code = NULL,
                         flow_direction = NULL,
                         reporter = NULL,
                         partner = NULL,
                         start_date = NULL,
                         end_date= NULL,
                         mode_of_transport = '0',
                         partner_2 = '0',
                         customs_code ='C00',
                         verbose = F,
                         ...) {

  frequency <- check_freq(frequency)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of frequency!"))
  }

  commodity_classification <- check_clCode(commodity_classification)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of commodity_classification."))
  }

  flow_direction <- check_flowCode(flow_direction)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of flow_direction."))
  }

  commodity_code <- check_cmdCode(commodity_code)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of commodity_code."))
  }

  reporter <- check_reporterCode(reporter)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of reporter."))
  }

  partner <- check_partnerCode(partner)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of partner."))
  }

  period <- check_date(start_date, end_date, frequency)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of start and end dates."))
  }

  params <- list(
    query_params = list(
      cmdCode = commodity_code,
      flowCode = flow_direction,
      partnerCode = partner,
      reporterCode = reporter,
      period = period,
      motCode = mode_of_transport,
      partner2Code = partner_2,
      customsCode = customs_code,
      ...
    ),
    url_params = list(freq = frequency,
                      clCode = commodity_classification)
  )

  return(params)
}

#' Check frequency parameter
#'
#' @param frequency A character string specifying the frequency of the data. Must be one of "A", "Q", or "M".
#'
#' @return A character string specifying the frequency of the data.
#'
#' @examplesIf interactive()
#' check_freq("A") # returns "A"
#' check_freq("Q") # returns "Q"
#' check_freq("M") # returns "M"
#' check_freq("D") # throws an error because "D" is not a valid frequency code
check_freq <- function(frequency) {
  rlang::arg_match(frequency, values = c('A', "Q", "M"))
  return(frequency)
}


#' Check HS classification parameter
#'
#' @param commodity_classification A character string specifying the HS classification code. Must be "HS".
#'
#' @return A character string specifying the HS classification code.
#'
#'
#' @examplesIf interactive()
#' untrader:::check_clCode("HS") # returns "HS"
#' untrader:::check_clCode("ISIC") # throws an error because "ISIC" is not a valid classification code
check_clCode <- function(commodity_classification) {
  rlang::arg_match(commodity_classification, values = c('HS'))
  return(commodity_classification)
}


#' Check flow_direction parameter
#'
#' @param flow_direction A character string or vector specifying the type of trade flow. Must be one or more of "import", "export", "re-export", "re-import", or "all".
#'
#' @return A character vector specifying the trade flow codes.
#'
#' @examplesIf interactive()
#' check_flowCode("import") # returns "M"
#' check_flowCode(c("export", "re-export")) # returns "X,RX"
#' check_flowCode("trade") # throws an error because "trade" is not a valid flow code
#' check_flowCode(NULL) # throws an error because at least one flow code must be provided
#'
check_flowCode <- function(flow_direction) {
  rlang::arg_match(
    flow_direction,
    values = c('import', 'export', 're-export', 're-import', 'all'),
    multiple = T
  )
  # check that flow_direction code is not null
  if (!is.null(flow_direction)) {
    flow_direction <- as.character(flow_direction)
  } else{
    rlang::abort("You need to provide at least one flow_direction reference.")
  }

  if(length(flow_direction)>1 & any(flow_direction=='all')){
    rlang::abort("You can only provide 'all' as a single argument")
  }

  if(length(flow_direction)>1|!any(flow_direction=='all')){
    flow_direction <- stringr::str_replace_all(flow_direction,'^import$',"M")
    flow_direction <- stringr::str_replace_all(flow_direction,'^export$',"X")
    flow_direction <- stringr::str_replace_all(flow_direction,'^re-import$',"RM")
    flow_direction <- stringr::str_replace_all(flow_direction,'^re-export$',"RX")
    flow_direction <- flow_direction |> paste0(collapse = ',')
  } else if( flow_direction=='all') {
    flow_direction <- 'M,X,RM,RX'
  }
  return(flow_direction)
}


#' Check commodity_code parameter
#'
#' @param commodity_code A character string or vector specifying the HS codes.
#'
#' @return A character vector specifying the HS codes.
#'
#' @examplesIf interactive()
#' check_cmdCode("01") # returns "01"
#' check_cmdCode(c("01", "02")) # returns "01,02"
#' check_cmdCode("ABC") # throws an error because "ABC" is not a valid HS code
#' check_cmdCode(NULL) # throws an error because at least one HS code must be provided
check_cmdCode <- function(commodity_code) {
  # check that commodity_code code is not null
  if (!is.null(commodity_code)) {
    commodity_code <- as.character(commodity_code)
  } else{
    rlang::abort("You need to provide at least one commodity_code reference.")
  }

  # check validity of arguments ---------------------------------------------
  # separating provided hs codes
  commodity_code <- stringr::str_squish(commodity_code)
  # if one of the HS codes is not in the list of valid HS codes send stop signal and list problems
  if (!all(commodity_code %in% untrader::HS$id)) {
    rlang::abort(paste0(
      "The following HS codes you provided are invalid: ",
      paste0(commodity_code[!commodity_code %in% untrader::HS$id], collapse = ", ")
    ))
  } else {
    commodity_code <- paste0(commodity_code, collapse = ',')
  }

  return(commodity_code)
}

#' Check reporter parameter
#'
#' This function checks that the given reporter code is valid. If the code is not
#' valid, the function throws an error message indicating which codes are invalid.
#' It also converts the input to a proper format if necessary.
#'
#' @param reporter A character vector or string of comma-separated codes that
#'   represent the reporters in the trade data. The default value is NULL.
#'
#' @return A character vector of valid reporter IDs.
#'
#' @examplesIf interactive()
#' check_reporterCode("USA")
#' check_reporterCode(c("USA", "FRA"))
#' check_reporterCode("all")
check_reporterCode <- function(reporter) {
  # check that reporter code is valid
  if (!is.null(reporter)) {
    reporter <- as.character(reporter)
  } else{
    rlang::abort("You need to provide at least one reporter")
  }


  ## check if valid reporter code length and type
  reporter <- stringr::str_squish(reporter)
  ## get multiple values or single values that are not 'all'
  if (length(reporter) > 1 | !any(reporter == 'all')) {
    if (any(reporter == 'all')) {
      rlang::abort('"all" can only be provided as a single argument')
    }
    # if one of the reporter codes is not in the list of valid reporter codes send stop signal and list problems
    if (!all(reporter %in% untrader::REPORTER$reporterCodeIsoAlpha3)) {
      rlang::abort(paste0(
        "The following reporterCodes you provided are invalid: ",
        paste0(reporter[!reporter %in% untrader::REPORTER$reporterCodeIsoAlpha3], collapse = ", ")
      ))
    }
  }

  # create proper ids for reporter Code
  if (length(reporter) > 1 | !any(reporter == 'all')) {
    reporter <-
      untrader::REPORTER$id[untrader::REPORTER$reporterCodeIsoAlpha3 %in% reporter &
                              untrader::REPORTER$isGroup ==
                              F] |>
      paste0(collapse = ',')
  } else if (reporter == 'all') {
    reporter <-
      untrader::REPORTER$id[untrader::REPORTER$isGroup == F] |>
      paste0(collapse = ',')
  }

  return(reporter)
}


#' Check partner parameter
#'
#' This function checks that the given partner code is valid. If the code is not
#' valid, the function throws an error message indicating which codes are invalid.
#' It also converts the input to a proper format if necessary.
#'
#' @param partner A character vector or string of comma-separated codes that
#'   represent the trade partners in the trade data. The default value is NULL.
#'
#' @return A character vector of valid partner IDs.
#'
#' @examplesIf interactive()
#' check_partnerCode("CAN")
#' check_partnerCode(c("CAN", "MEX"))
#' check_partnerCode("all")
check_partnerCode <- function(partner) {
  # check that partner code is valid
  if (!is.null(partner)) {
    partner <- as.character(partner)
  } else{
    rlang::abort("You need to provide at least one partner")
  }

  if (length(partner) > 1 | !any(partner == 'all')) {
    partner <- stringr::str_squish(partner)
    if (any(partner == 'all')) {
      rlang::abort('"all" can only be provided as a single argument')
    }
    # if one of the partnerCodes is not in the list of valid partnerCodes send stop signal and list problems
    if (!all(partner %in% c(untrader::PARTNER$PartnerCodeIsoAlpha3, 'world'))) {
      rlang::abort(paste0(
        "The following partner you provided are invalid: ",
        paste0(partner[!partner %in% c(untrader::PARTNER$PartnerCodeIsoAlpha3, 'world')], collapse = ", ")
      ))
    }
  }

  # create proper ids for partner
  if (length(partner) > 1 | !any(partner == 'all')) {
    values <-
      untrader::PARTNER$id[untrader::PARTNER$PartnerCodeIsoAlpha3 %in% partner &
                             untrader::PARTNER$isGroup == F] |>
      paste0(collapse = ',')

    if (any(stringr::str_detect(partner, 'world'))) {
      partner <- paste0(values, '0', collapse = "")
    } else {
      partner <- values
    }

  } else if (partner == 'world') {
    partner <- '0'
  } else if (partner == 'all') {
    partner <-
      untrader::PARTNER$id[untrader::PARTNER$isGroup == F] |>
      paste0(collapse = ',')
  }
  return(partner)
}


## the get date range function was taken from https://github.com/ropensci/comtradr/blob/master/tests/testthat/test-ct_search.R

#' Check date parameter
#'
#' This function checks that the given period code is valid. If the range or format is not valid, the function throws an error message indicating which codes are invalid. It also converts the input to the proper format if necessary.
#'
#' @param start_date The start date of the query, either in the format `yyyy` or `yyyy-mm`.
#' @param end_date The end date of the query, either in the format `yyyy` or `yyyy-mm`. Can be a maximum of 12 years after the start date for the annuel frequency or one year for monthly.
#' @param frequency The frequency of reported trade data, either `A` for annual or `M` for monthly.
#'
#' @return A character vector of valid reporter IDs.
#'
#' @examplesIf interactive()
#' check_date(2010,2011,'A')
check_date <- function(start_date, end_date, frequency) {
  start_date <- as.character(start_date)
  end_date <- as.character(end_date)

  if (frequency == "A") {
    # Date range when freq is "annual" (date range by year).
    start_date <- convert_to_date(date_obj = start_date)
    end_date <- convert_to_date(date_obj = end_date)
    date_range <- seq.Date(start_date, end_date, by = "year") |>
      format(format = "%Y")
  } else if (frequency == "M") {
    # Date range when freq is "monthly".
    sd_year <- is_year(start_date)
    ed_year <- is_year(end_date)
    if (sd_year && ed_year) {
      # If start_date and end_date are both years ("yyyy") and are identical,
      # return the single year as the date range.
      if (identical(start_date, end_date)) {
        return(start_date)
      } else {
        rlang::abort("Cannot get more than a single year's worth of monthly data in a single query")
      }
    } else if (!sd_year && !ed_year) {
      # If neither start_date nor end_date are years, get date range by month.
      start_date <- convert_to_date(start_date)
      end_date <- convert_to_date(end_date)
      date_range <- seq.Date(start_date, end_date, by = "month") |>
        format(format = "%Y%m")
    } else {
      # Between start_date and end_date, if one is a year and the other isn't,
      # throw an error.
      rlang::abort("If arg 'frequency' is 'monthly', 'start_date' and 'end_date' must have the same format")
    }
  }

  # If the derived date range is longer than five elements, throw an error.
  if (length(date_range) > 12) {
    stop("If specifying years/months, cannot search more than five consecutive years/months in a single query")
  }

  return(paste(date_range, collapse = ","))
}


#' Given a numeric or character date, convert to an object with class "Date".
#'
#' @return Object of class "Date" (using base::as.Date()).
#' @noRd
convert_to_date <- function(date_obj) {
  # Convert to Date.
  if (is_year(x = date_obj)) {
    date_obj <- as.Date(paste0(date_obj, "-01-01"), format = "%Y-%m-%d")
  } else if (is_year_month(x = date_obj)) {
    date_obj <- as.Date(paste0(date_obj, "-01"), format = "%Y-%m-%d")
  } else {
    date_obj <- as.Date(date_obj, format = "%Y-%m-%d")
  }
  # If conversion to Date failed, throw error.
  if (is.na(date_obj)) {
    rlang::abort(
      paste("arg must be a date with one of these formats:\n",
            "int: yyyy\n",
            "char: 'yyyy'\n",
            "char: 'yyyy-mm'\n",
            "char: 'yyyy-mm-dd'"))
  }

  return(date_obj)
}


#' Is input a year string or not.
#'
#' @noRd
is_year <- function(x) {
  grepl("^\\d{4}$", x)
}


#' Is input a year-month string or not.
#'
#' @noRd
is_year_month <- function(x) {
  grepl("^\\d{4}-\\d{2}", x)
}
