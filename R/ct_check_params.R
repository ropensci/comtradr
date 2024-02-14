#' Check input parameters are valid and in compliance with UN Comtrade APIs.
#'
#' This is an internal function that is called by `ct_get_data()` and processes
#' most arguments that are passed to it according to the relevant limitations
#' of the official Comtrade API.
#'
#'
#' @noRd
#' @returns Returns a list of named parameters for building a request.
#' @inheritParams ct_get_data
ct_check_params <- function(type,
                            frequency,
                            commodity_classification,
                            commodity_code,
                            flow_direction,
                            reporter,
                            partner,
                            start_date,
                            end_date,
                            mode_of_transport,
                            partner_2,
                            customs_code,
                            update,
                            verbose,
                            extra_params,
                            bulk) {

  type <- check_type(type)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of type."))
  }

  commodity_classification <- check_clCode(type, commodity_classification,bulk)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of commodity_classification."))
  }

  if(!bulk){
    frequency <- check_freq(type, frequency)
    if (verbose) {
      cli::cli_inform(c("v" = "Checked validity of frequency."))
    }


    flow_direction <- check_flowCode(flow_direction, update, verbose)
    if (verbose) {
      cli::cli_inform(c("v" = "Checked validity of flow_direction."))
    }

    commodity_code <- check_cmdCode(commodity_classification,
                                    commodity_code,
                                    update = update,
                                    verbose = verbose
    )
    if (verbose) {
      cli::cli_inform(c("v" = "Checked validity of commodity_code."))
    }

    partner <- check_partnerCode(partner, update = update, verbose = verbose)
    if (verbose) {
      cli::cli_inform(c("v" = "Checked validity of partner."))
    }
    partner_2 <- check_partner2Code(partner_2, update = update, verbose = verbose)
    if (verbose) {
      cli::cli_inform(c("v" = "Checked validity of partner_2."))
    }

    mode_of_transport <- check_motCode(mode_of_transport,
                                       update = update,
                                       verbose = verbose
    )
    if (verbose) {
      cli::cli_inform(c("v" = "Checked validity of mode_of_transport."))
    }

    customs_code <- check_customsCode(customs_code,
                                      update = update,
                                      verbose = verbose
    )
    if (verbose) {
      cli::cli_inform(c("v" = "Checked validity of customs_code."))
    }
  }




  reporter <- check_reporterCode(reporter, update = update, verbose = verbose)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of reporter."))
  }

  period <- check_date(start_date, end_date, frequency, bulk)
  if (verbose) {
    cli::cli_inform(c("v" = "Checked validity of start and end dates."))
  }

  if(!bulk){
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
        includeDesc = "TRUE"
      ),
      url_params = list(
        type = type,
        freq = frequency,
        clCode = commodity_classification
      ),
      extra_params = list(
        extra_params = extra_params
      )
    )
  } else {
    params <- list(
      query_params = list(
        reporterCode = reporter,
        period = period
      ),
      url_params = list(
        type = type,
        freq = frequency,
        clCode = commodity_classification
      ),
      extra_params = list(
        extra_params = extra_params
      )
    )
  }

  return(params)
}

#' Check validity of type parameter.
#'
#' Trade frequency: 'goods' for goods and 'services' for services
#'
#' @inheritParams get_comtrade_data
#'
#' @returns A character string specifying the type parameter of the data.
#'
#'
#' @noRd
check_type <- function(type) {
  lowercase <- tolower(type)
  rlang::arg_match(lowercase, values = c("goods", "services"))
  switch <- c(
    "goods" = "C",
    "services" = "S"
  )

  type <- switch[lowercase
  ]
  return(type)
}


#' Check validity of frequency parameter.
#'
#' Trade frequency: 'A' for annual and 'M' for monthly.
#'
#' @inheritParams ct_get_data
#'
#' @returns A character string specifying the frequency of the data.
#'
#'
#' @noRd
check_freq <- function(type, frequency) {
  # only annual data for services endpoint
  if (type == "S") {
    rlang::arg_match(frequency, values = c("A"))
  } else {
    rlang::arg_match(frequency, values = c("A", "M"))
  }
  return(frequency)
}

#' Check validity of classification parameter.
#'
#' Trade (IMTS) classifications: HS, SITC, BEC or EBOPS.
#' Currently, we only support the HS classification.
#'
#' @inheritParams ct_get_data
#'
#' @returns A character string specifying the selected classification code.
#'
#'
#' @noRd
check_clCode <- function(type, commodity_classification, bulk) {

  cmd_list_goods <- c("HS", "S1", "S2", "S3", "S4", "SS", "B4", "B5")
  cmd_list_services <- c("EB02", "EB10", "EB10S", "EB")

  if(bulk){
    cmd_list_goods <- c(cmd_list_goods,"H6","H5","H4","H3","H2","H1","H0")
  }

  if (type == "C") {
    rlang::arg_match(commodity_classification, values = cmd_list_goods)
  } else {
    rlang::arg_match(commodity_classification, values = cmd_list_services)
  }
  return(commodity_classification)
}

#' Check validity of flow direction parameter.
#'
#' Trade flow code: export, import, re-export, re-import.
#'
#' @inheritParams ct_get_data
#'
#' @returns A character vector specifying the trade flow codes.
#'
#'
#' @noRd
check_flowCode <- function(flow_direction, update, verbose) {
  id <- text <- NULL
  # if 'everything' is selected, return NULL, which in the API
  # equals to all possible values
  if (!any(flow_direction %in% "everything")) {
    flow_direction <- as.character(flow_direction)

    # remove any white space from cmd codes provided
    flow_direction <- stringr::str_squish(flow_direction) |> tolower()

    # get the list of valid parameters from inst/extdata
    valid_codes <-
      ct_get_ref_table(
        dataset_id = "flow_direction",
        update = update,
        verbose = verbose
      ) |>
      poorman::mutate(text = tolower(text))

    rlang::arg_match(flow_direction, values = valid_codes$text, multiple = TRUE)


      flow_direction <- valid_codes |>
        poorman::filter(text %in% flow_direction) |>
        poorman::pull(id) |>
        paste0(collapse = ",")

  } else {
    flow_direction <- NULL
  }
  return(flow_direction)
}

#' Check validity of commodity code parameter.
#'
#' Commodity code. We currently only support HS codes.
#'
#' @inheritParams ct_get_data
#'
#' @returns A character vector specifying the commodity codes requested.
#'
#'
#' @noRd
check_cmdCode <-
  function(commodity_classification,
           commodity_code,
           update = FALSE,
           verbose = FALSE) {
    # check that commodity_code code is not null
    if (!any(commodity_code %in% "everything")) {
      commodity_code <- as.character(commodity_code)

      # remove any white space from cmd codes provided
      commodity_code <- stringr::str_squish(commodity_code)

      # get the list of valid parameters from inst/extdata
      valid_codes <-
        ct_get_ref_table(
          dataset_id = commodity_classification,
          update = update,
          verbose = verbose
        )$id

      # if one of the codes is not in the list of valid codes
      # send stop signal and list problems
      if (!all(commodity_code %in% valid_codes)) {
        rlang::abort(paste0(
          "The following services/commodity codes you provided are invalid: ",
          paste0(setdiff(commodity_code, valid_codes), collapse = ", ")
        ))
      } else {
        commodity_code <- paste0(commodity_code, collapse = ",")
      }
    } else {
      commodity_code <- NULL
    }

    return(commodity_code)
  }

#' Check validity of reporter parameter.
#'
#' This function checks that the given reporter code is valid.
#' If the code is not valid, the function throws an error message
#' indicating which codes are invalid. It also converts the input to a
#' proper format if necessary.
#'
#' @inheritParams ct_get_data
#'
#' @returns A character vector of valid reporter IDs.
#'
#' @noRd
check_reporterCode <- function(reporter, update = FALSE, verbose = FALSE) {
  iso_3 <- id <- group <- NULL
  # check that reporter code is valid
  if (!any(reporter %in% "everything")) {
    reporter <- as.character(reporter)

    ## check if valid reporter code length and type
    reporter <- stringr::str_squish(reporter)

    reporter_codes <-
      ct_get_ref_table(
        dataset_id = "reporter",
        update = update,
        verbose = verbose
      )

    ## get multiple values or single values that are not 'all_countries'
    if (length(reporter) > 1 | !any(reporter %in% "all_countries")) {
      if (any(reporter == "all_countries")) {
    rlang::abort('"all_countries" can only be provided as a single argument.')
      }
      # if one of the reporter codes is not in the list of valid reporter codes
      # send stop signal and list problems
      if (!all(reporter %in% reporter_codes$iso_3)) {
        rlang::abort(paste0(
          "The following reporter(s) you provided are invalid: ",
          paste0(setdiff(reporter, reporter_codes$iso_3), collapse = ", ")
        ))
      }
    }

    # create proper ids for reporter Code
    if (length(reporter) > 1 | !any(reporter %in% "all_countries")) {
      reporter <- reporter_codes |>
        poorman::filter(iso_3 %in% reporter) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    } else if (reporter == "all_countries") {
      reporter <- reporter_codes |>
        poorman::filter(group == FALSE) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    }
  } else {
    reporter <- NULL
  }

  return(reporter)
}

#' Check validity of partner parameter.
#'
#' This function checks that the given partner code is valid.
#' If the code is not valid, the function throws an error message
#' indicating which codes are invalid. It also converts the input to a
#' proper format if necessary.
#'
#' @inheritParams ct_get_data
#'
#' @returns A character vector of valid partner IDs.
#'
#'
#' @noRd
check_partnerCode <- function(partner, update = FALSE, verbose = FALSE) {
  ## evade checks in RMD Check about 'no visible binding...'
  iso_3 <- id <- group <- NULL

  # check that partner code is valid
  if (!any(partner %in% "everything")) {
    partner <- as.character(partner)

    partner_codes <- ct_get_ref_table(
      dataset_id = "partner",
      update = update, verbose = verbose
    )


    if (length(partner) > 1 | !any(partner %in% "all_countries")) {
      partner <- stringr::str_squish(partner)
      if (any(partner %in% "all_countries")) {
      rlang::abort('"all_countries" can only be provided as a single argument.')
      }
      # if one of the partnerCodes is not in the list of valid partnerCodes
      # send stop signal and list problems
      if (!all(partner %in% partner_codes$iso_3)) {
        rlang::abort(paste(
          "The following partner you provided are invalid: ",
          setdiff(partner, partner_codes$iso_3),
          collapse = ", "
        ))
      }
    }

    # create proper ids for partner
    if (length(partner) > 1 | !any(partner %in% "all_countries")) {
      partner <- partner_codes |>
        poorman::filter(iso_3 %in% partner) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    } else if (partner == "all_countries") {
      partner <- partner_codes |>
        poorman::filter(group == FALSE) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    }
  } else {
    partner <- NULL
  }

  return(partner)
}


#' Check validity of partner_2 parameter.
#'
#' This function checks that the given partner_2 code is valid.
#' If the code is not valid, the function throws an error message
#' indicating which codes are invalid. It also converts the input
#' to a proper format if necessary.
#'
#' @inheritParams ct_get_data
#'
#' @returns A character vector of valid partner_2 IDs.
#'
#' @noRd
check_partner2Code <- function(partner, update = FALSE, verbose = FALSE) {
  iso_3 <- id <- group <- NULL

  # check that partner code is valid
  if (!any(partner %in% "everything")) {
    partner <- as.character(partner)


    partner_codes <- ct_get_ref_table(
      dataset_id = "partner",
      update = update, verbose = verbose
    )


    if (length(partner) > 1 | !any(partner == "all_countries")) {
      partner <- stringr::str_squish(partner)
      if (any(partner == "all_countries")) {
      rlang::abort('"all_countries" can only be provided as a single argument.')
      }
      # if one of the partnerCodes is not in the list
      #of valid partnerCodes send stop signal and list problems
      if (!all(partner %in% partner_codes$iso_3)) {
        rlang::abort(paste(
          "The following partner_2 you provided are invalid: ",
          setdiff(partner, partner_codes$iso_3),
          collapse = ", "
        ))
      }
    }

    # create proper ids for partner
    if (length(partner) > 1 | !any(partner == "all_countries")) {
      partner <- partner_codes |>
        poorman::filter(iso_3 %in% partner) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    } else if (partner == "all_countries") {
      partner <- partner_codes |>
        poorman::filter(group == FALSE) |>
        poorman::pull(id) |>
        paste(collapse = ",")
    }
  } else {
    partner <- NULL
  }

  return(partner)
}


#' Check validity of mode of transport parameter.
#'
#'
#' @inheritParams ct_get_data
#'
#' @returns A character vector specifying the modes of transport requested.
#'
#' @noRd
check_motCode <-
  function(mode_of_transport,
           update = FALSE,
           verbose = FALSE) {
    # check that commodity_code code is not null
    id <- text <- NA
    if (!any(mode_of_transport %in% "everything")) {
      valid_codes <-
        ct_get_ref_table(
          dataset_id = "mode_of_transport",
          update = update,
          verbose = verbose
        )
      ## check whether "everything" is selected

        mode_of_transport <- as.character(mode_of_transport)

        # remove any white space from cmd codes provided
        mode_of_transport <- stringr::str_squish(mode_of_transport)

        # get the list of valid parameters from inst/extdata


        # if one of the codes is not in the list of valid codes
        # send stop signal and list problems
        if (!all(mode_of_transport %in% valid_codes$text)) {
          rlang::abort(
            paste0(
            "The following mode_of_transport codes you provided are invalid: ",
              paste0(
                setdiff(mode_of_transport, valid_codes$text),
                collapse = ", "
              )
            )
          )
        } else {
          mode_of_transport <- valid_codes |>
            poorman::filter(text %in% mode_of_transport) |>
            poorman::summarise(id = paste0(id, collapse = ","))
        }
      mode_of_transport <- mode_of_transport$id
    } else {
      mode_of_transport <- NULL
    }

    return(mode_of_transport)
  }

#' Check validity of customs parameter.
#'
#'
#' @inheritParams ct_get_data
#'
#' @returns A character vector specifying the custom codes requested.
#'
#' @noRd
check_customsCode <- function(customs_code, update = FALSE, verbose = FALSE) {
  # check that commodity_code code is not null
  if (!any(customs_code %in% "everything")) {
    customs_code <- as.character(customs_code)

    # remove any white space from cmd codes provided
    customs_code <- stringr::str_squish(customs_code)

    # get the list of valid parameters from inst/extdata
    valid_codes <- ct_get_ref_table(
      dataset_id = "customs_code",
      update = update,
      verbose = verbose
    )$id

    # if one of the codes is not in the list of valid codes
    # send stop signal and list problems
    if (!all(customs_code %in% valid_codes)) {
      rlang::abort(paste0(
        "The following customs_code codes you provided are invalid: ",
        paste0(setdiff(customs_code, valid_codes), collapse = ", ")
      ))
    } else {
      customs_code <- paste0(customs_code, collapse = ",")
    }
  } else {
    customs_code <- NULL
  }

  return(customs_code)
}


## the get date range function was taken from https://github.com/ropensci/comtradr/blob/master/tests/testthat/test-ct_search.R # nolint

#' Check validity of date parameter.
#'
#' This function checks that the given period code is valid. If the range or
#' format is not valid, the function throws an error message indicating which
#' codes are invalid. It also converts the input to the proper format if
#' necessary.
#'
#' @inheritParams ct_get_data
#'
#' @returns A character vector of valid reporter IDs.
#'
#' @noRd
check_date <- function(start_date, end_date, frequency, bulk) {
  if (is.null(start_date) | is.null(end_date)) {
rlang::abort("Please provide a start and end date for the period of interest.")
  }

  start_date <- as.character(start_date)
  end_date <- as.character(end_date)

  if (frequency == "A") {
    # Date range when freq is "annual" (date range by year)
    start_date <- convert_to_date(date_obj = start_date)
    end_date <- convert_to_date(date_obj = end_date)
    date_range <- seq.Date(start_date, end_date, by = "year") |>
      format(format = "%Y")
  } else if (frequency == "M") {
    # Date range when freq is "monthly"
    sd_year <- is_year(start_date)
    ed_year <- is_year(end_date)
    if (sd_year && ed_year) {
      # If start_date and end_date are both years ("yyyy") and are identical,
      # return the single year as the date range.
      start_date <- convert_to_date(start_date)
      end_date <- convert_to_date(end_date)
      if (!bulk) {

        if (identical(start_date, end_date)) {
          date_range <-
            seq.Date(start_date, by = "month", length.out = 12) |>
            format(format = "%Y%m")
        } else {
          rlang::abort("Cannot get more than a single year's worth of monthly data in a single query.") # nolint
        }
      } else {
        date_range <-
          seq.Date(from = start_date, to = end_date, by = "month") |>
          format(format = "%Y%m")
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
      rlang::abort("If arg 'frequency' is 'monthly', 'start_date' and 'end_date' must have the same format.") # nolint
    }
  }

  # If the derived date range is longer than five elements, throw an error.
  if (!bulk && length(date_range) > 12) {
    rlang::abort("If specifying years/months, cannot search more than twelve consecutive years/months in a single query.") # nolint
  }

  return(paste(date_range, collapse = ","))
}


#' Given a numeric or character date, convert to an object with class "Date".
#'
#' @returns Object of class "Date" (using base::as.Date()).
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
      paste(
        "arg must be a date with one of these formats:\n",
        "int: yyyy\n",
        "char: 'yyyy'\n",
        "char: 'yyyy-mm'\n",
        "char: 'yyyy-mm-dd'"
      )
    )
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
