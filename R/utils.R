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
#' @param dataset_id The dataset ID, which is either partner, reporter or a valid classification scheme.
#'
#' @export
ct_get_ref_table <- function(dataset_id, update = F, verbose = F) {
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
    'SS'    = 'cmd_ss'   ,
    'reporter'    = 'reporter'   ,
    'partner'    = 'partner'

  )
  rlang::arg_match(dataset_id, values = names(switch_list))

  ref_table_id <- switch_list[dataset_id]

  data <- get(dataset_id, envir = ct_env)
  if(is.null(data)){
    data <- fs::path_package(paste0('extdata/',ref_table_id,'.rds'),package = 'comtradr') |>
      readr::read_rds()
    assign(dataset_id,data,envir = ct_env)
  }
  if(update){
    if (verbose) {
      cli::cli_inform(c("i" = paste0("Attempting to update reference table: ",dataset_id)))
    }
    data_new <- ct_download_ref_table(ref_table_id = ref_table_id)
    if(unique(data_new$last_modified)>unique(data$last_modified)){
      if (verbose) {
        cli::cli_inform(c("i" = paste0("Updated reference tables ",dataset_id," with new data, last modified on: ",last_modified)))
      }
      assign(dataset_id,data_new,envir = ct_env)
      return(data_new)
    } else {
      if (verbose) {
        cli::cli_inform(c("i" = paste0('No update necessary for table ',dataset_id,'.')))
      }
      return(data)
    }
  } else {
      return(data)
    }
  }



#' Downloading the references tables from UN Comtrade
#'
#' @noRd
ct_download_ref_table <- function(ref_table_id) {
  datasets <- get('list_of_datasets', envir = ct_env)
  if (is.null(datasets)) {
    path_datasets <-
      fs::path_package('extdata/list_of_datasets.rda', package = 'comtradr')
    load(path_datasets, envir = ct_env)
  }
  datasets <- get('list_of_datasets', envir = ct_env) |>
    poorman::filter(.data$category == ref_table_id)

  response <- httr2::request(datasets$fileuri) |>
    httr2::req_perform()

  data <- response |>
    httr2::resp_body_json(simplifyVector = T)

  last_modified <-
    httr2::resp_header(header = "Last-Modified", resp = response) |>
    stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
    as.Date(format = "%d %b %Y")

  data <- data$results

  data$last_modified <- last_modified

  if (ref_table_id %in% c('reporter', 'partner')) {
    if (ref_table_id == 'reporter') {
      data <- data |>
        poorman::transmute(
          .data$id,
          country = .data$text,
          iso_3 = .data$reporterCodeIsoAlpha3,
          entry_year = lubridate::year(.data$entryEffectiveDate),
          exit_year = lubridate::year(.data$entryExpiredDate),
          group = .data$isGroup,
          .data$last_modified
        )
    } else {
      data <- data |>
        poorman::transmute(
          .data$id,
          country = .data$text,
          iso_3 = .data$PartnerCodeIsoAlpha3,
          entry_year = lubridate::year(.data$entryEffectiveDate),
          exit_year = lubridate::year(.data$entryExpiredDate),
          group = .data$isGroup,
          .data$last_modified
        ) |>
        poorman::mutate(.data$iso_3 = ifelse(country == 'World', 'World', .data$iso_3))
    }
    return(data)
  } else {
    return(data)
  }
}



