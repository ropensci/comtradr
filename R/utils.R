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
#'
#' The first time, the function will read from disk, the second time from the environment. In the case of a necessary update the new data will be saved to the environment for the current session.
#'
#' @param dataset_id The dataset ID, which is either partner, reporter or a valid classification scheme.
#' @inheritParams ct_get_data
#' @export
ct_get_ref_table <- function(dataset_id, update = F, verbose = F) {

  ## make switch to the name of the datasets, which are slightly different to the dataset_ids
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
    'partner'    = 'partner',
    'mot'    = 'mot',
    'customs'    = 'customs'

  )

  ## check dataset id for valid values
  rlang::arg_match(dataset_id, values = names(switch_list))

  ## assign the proper file name to a new variable
  ref_table_id <- switch_list[dataset_id]

  ## attempt to return the data from the environment first
  data <- get(dataset_id, envir = ct_env)

  ## if the dataset is not yet loaded into the environment read it from disk and save to environment
  if(is.null(data)){
    data <- fs::path_package(paste0('extdata/',ref_table_id,'.rds'),package = 'comtradr') |>
      readr::read_rds()
    assign(dataset_id,data,envir = ct_env)
  }

  if(update & any(dataset_id %in% ct_env$updated)){
    ## if update is true, but dataset_id has already been updated once only return message
    if (verbose) {
      cli::cli_inform(c("i" = paste0("Already checked for updates for ",dataset_id,' in this session.')))
    }
    return(data)
  } else if(update){
    ## if update is true and not yet updated in this session inform user that update process is starting
    if (verbose) {
      cli::cli_inform(c("i" = paste0("Attempting to update reference table: ",dataset_id)))
    }

    ## download new reference table from the UN
    data_new <- ct_download_ref_table(ref_table_id = ref_table_id)

    if(unique(data_new$last_modified)>unique(data$last_modified)){
      ## if the date last modified, returned in the header is newer than the old data
      if (verbose) {
        cli::cli_inform(c("i" = paste0("Updated reference tables ",dataset_id," with new data, last modified on: ",unique(data_new$last_modified))))
      }

      ## write to environment and overwrite old data
      assign(dataset_id,data_new,envir = ct_env)

      ## let environment variable know that dataset has been updated
      ct_env$updated <- c(ct_env$updated,dataset_id)

      return(data_new)
    } else {
      ## if last_modified is not newer, let user know that datasets are up to date.
      if (verbose) {
        cli::cli_inform(c("i" = paste0('No update necessary for table ',dataset_id,'.')))
      }

      ## save in env variable, that update has been checked in this session
      ct_env$updated <- c(ct_env$updated,dataset_id)

      return(data)
    }
  } else {
    ## if no update parameter passed on, just return the data read from disk or the env
      return(data)
    }
  }



#' Downloading the references tables from UN Comtrade
#'
#' @noRd
ct_download_ref_table <- function(ref_table_id) {
  iso_3 <- id <- group <- category <- text <- reporterCodeIsoAlpha3 <- entryEffectiveDate <-  NULL
  entryExpiredDate <- isGroup <- PartnerCodeIsoAlpha3 <- country <-  NULL

  ## attempt to get list of datasets of the UN from the env
  datasets <- get('list_of_datasets', envir = ct_env)
  if (is.null(datasets)) {
    ## if not in env read from disk
    path_datasets <-
      fs::path_package('extdata/list_of_datasets.rda', package = 'comtradr')
    load(path_datasets, envir = ct_env)
  }

  ## filter to queried ref_table
  datasets <- get('list_of_datasets', envir = ct_env) |>
    poorman::filter(category == ref_table_id)

  ## download reference table from UN
  response <- httr2::request(datasets$fileuri) |>
    httr2::req_perform()

  ## parse response
  data <- response |>
    httr2::resp_body_json(simplifyVector = T)

  ## get date of last modification from headers
  last_modified <-
    httr2::resp_header(header = "Last-Modified", resp = response) |>
    stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
    as.Date(format = "%d %b %Y")

  ## get results from json file
  data <- data$results

  ## write last modification time to ref table
  data$last_modified <- last_modified

  ## cleaning for reporter and partner
  if (ref_table_id %in% c('reporter', 'partner')) {
    if (ref_table_id == 'reporter') {
      data <- data |>
        poorman::transmute(
          id,
          country = text,
          iso_3 = reporterCodeIsoAlpha3,
          entry_year = lubridate::year(entryEffectiveDate),
          exit_year = lubridate::year(entryExpiredDate),
          group = isGroup,
          last_modified
        )
    } else {
      data <- data |>
        poorman::transmute(
          id,
          country = text,
          iso_3 = PartnerCodeIsoAlpha3,
          entry_year = lubridate::year(entryEffectiveDate),
          exit_year = lubridate::year(entryExpiredDate),
          group = isGroup,
          last_modified
        ) |>
        poorman::mutate(iso_3 = ifelse(country == 'World', 'World', iso_3))
    }
    return(data)
  } else {
    return(data)
  }
}



