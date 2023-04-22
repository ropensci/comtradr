#' Set your primary Comtrade API key in the environment variable
#'
#' If you would like your Comtrade API key to persist in between sessions, use `usethis::edit_r_environ()` to add the env variable COMTRADE_PRIMARY to your environment file.
#'
#' @param key Provide your primary comtrade key
#'
#' @return Saves your comtrade primary key in the environment.
#' @export
set_primary_comtrade_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("COMTRADE_PRIMARY" = key)
}

#' get_primary_comtrade_key
#'
#' If you would like your Comtrade API key to persist in between sessions, use `usethis::edit_r_environ()` to add the env variable COMTRADE_PRIMARY to your environment file.
#'
#' @return Gets your primary comtrade key from the environment var COMTRADE_PRIMARY
#' @export
get_primary_comtrade_key <- function() {
  key <- Sys.getenv("COMTRADE_PRIMARY")
  if (!identical(key, "")) {
    return(key)
  } else {
    rlang::abort("No API key found, please supply with `set_primary_comtrade_key` function or set COMTRADE_PRIMARY env var")
  }
}

#' Get reference table from package data
#' @inheritParams ct_get_data
#'
#' @export
ct_get_commodity_table <- function(commodity_classification) {
  switch_list <- c(
    'B4'    = 'cmd_b4'   ,
    'B5'    = 'cmd_b5'   ,
    'EB02'  = 'cmd_eb02' ,
    'EB10'  = 'cmd_eb10' ,
    'EB10S' = 'cmd_eb10s',
    'EB'    = 'cmd_eb'   ,
    'HS'    = 'cmd_hs'   ,
    'S1'    = 'cmd_s1'   ,
    'S2'    = 'cmd_s2'   ,
    'S3'    = 'cmd_s3'   ,
    'S4'    = 'cmd_s4'   ,
    'SS'    = 'cmd_ss'
  )

  possible_values <- names(switch_list)
  rlang::arg_match(commodity_classification, values = possible_values)

  ref_table_name <- switch_list[commodity_classification]

  data <- get(commodity_classification, envir = ct_env)
  if(!is.null(data)){
    return(data)
  } else {
    data <- fs::path_package(paste0('extdata/',ref_table_name,'.rds'),package = 'comtradr') |>
      readr::read_rds()
    assign(commodity_classification,data,envir = ct_env)
    return(data)
  }
}





#' Get commodity database
#'
#' Helper function that attempts to return the commodity DB as a data frame.
#' It first looks in the pkg env "ct_env", if that fails it will look for the
#' pkg file "commodity_table.rda". If both fail an error will be thrown
#' telling the user how to download the commodity DB file (just a matter of
#' running ct_update_databases(force = TRUE)).
#'
#' @noRd
get_commodity_db <- function() {
  # Look in ct_env.
  df <- get("commodity_df", envir = ct_env)
  if (!is.null(df)) {
    return(df)
  } else {
    # If no commodity_df in ct_env, look for commodity_table file.
    commodity_file <- system.file("extdata",
                                  "commodity_table.rda",
                                  package = "comtradr")
    if (file.exists(commodity_file)) {
      load(commodity_file, envir = ct_env)
      df <- get("commodity_df", envir = ct_env)
      return(df)
    } else {
      stop(missing_file_msg("commodity"), call. = FALSE)
    }
  }
}


#' Get country database
#'
#' Helper function that will attempt to return the country DB as a data frame.
#' It first looks in the pkg env "ct_env", if that fails it will look for the
#' pkg file "commodity_table.rda". If both fail an error will be thrown
#' telling the user how to download the commodity DB file (just a matter of
#' running ct_update_databases(force = TRUE)).
#'
#' @noRd
get_country_db <- function() {
  # Look in ct_env.
  df <- get("country_df", envir = ct_env)
  if (!is.null(df)) {
    return(df)
  } else {
    # If no country_df in ct_env, look for country_table file.
    country_file <- system.file("extdata",
                                "country_table.rda",
                                package = "comtradr")
    if (file.exists(country_file)) {
      load(country_file, envir = ct_env)
      df <- get("country_df", envir = ct_env)
      return(df)
    } else {
      stop(missing_file_msg("country"), call. = FALSE)
    }
  }
}


#' Missing DB file message
#'
#' @noRd
missing_file_msg <- function(type) {
  paste0(type, " database file not found. In order to download the ",
         type, " database from Comtrade, run:\n",
         "ct_update_databases(force = TRUE)")
}


#' Switch for getting the proper commodity type string, used when constructing
#' the api call url in function ct_search
#'
#' @noRd
commodity_type_switch <- function(commodity_type) {
  dict <- c(
    "HS" = "HS",
    "HS1992" = "H0",
    "HS1996" = "H1",
    "HS2002" = "H2",
    "HS2007" = "H3",
    "HS2012" = "H4",
    "HS2017" = "H5",
    "SITC" = "ST",
    "SITCrev1" = "S1",
    "SITCrev2" = "S2",
    "SITCrev3" = "S3",
    "SITCrev4" = "S4",
    "BEC" = "BEC"
  )

  dict[commodity_type]
}

