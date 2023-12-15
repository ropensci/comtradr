#' Set your primary Comtrade API key in the environment variable
#'
#' If you would like your Comtrade API key to persist in between sessions, use `usethis::edit_r_environ()` to add the env variable COMTRADE_PRIMARY to your environment file.
#'
#' @param key Provide your primary comtrade key
#'
#' @returns Saves your comtrade primary key in the environment.
#' @export
#' @examplesIf interactive()
#' ## set API key
#' set_primary_comtrade_key('xxxxxc678ca4dbxxxxxxxx8285r3')
#'
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
#' @returns Gets your primary comtrade key from the environment var COMTRADE_PRIMARY
#' @export
#' @examplesIf interactive()
#' ## get API key
#' get_primary_comtrade_key()
#'
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
#' The first time, the function will read from disk, the second time from the
#' environment. In the case of a necessary update the new data will be saved
#' to the environment for the current session.
#' You can use this table to look at the reference tables and if necessary
#' extract respective classification codes by hand. In general we would
#' recommend the function `ct_commodity_lookup` for this purpose.
#' It uses the present function in the backend.
#'
#' @details The function allows you to query most possible input parameters
#' that are listed by the Comtrade API. The following dataset_ids are permitted:
#' * Datasets that contain codes for the `commodity_code` argument. The name is
#' the same as you would provide under `commodity_classification`.
#'   * 'HS' This is probably the most common classification for goods.
#'   * 'B4'
#'   * 'B5'
#'   * 'EB02'
#'   * 'EB10'
#'   * 'EB10S'
#'   * 'EB'
#'   * 'S1'
#'   * 'S2'
#'   * 'S3'
#'   * 'S4'
#'   * 'SS'
#'
#' * 'reporter'
#' * 'partner'
#' * 'mode_of_transport'
#' * 'customs_code'
#'
#' @param dataset_id The dataset ID, which is either partner,
#' reporter or a valid classification scheme.
#' @inheritParams ct_get_data
#' @export
#' @returns a tidy dataset with a reference table
#'
#' @examplesIf interactive()
#' ## get HS commodity table
#' ct_get_ref_table("HS")
#'
#' ## get reporter table
#' ct_get_ref_table("reporter")
#'
ct_get_ref_table <- function(dataset_id, update = FALSE, verbose = FALSE) {

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
    'mode_of_transport'    = 'mot',
    'customs_code'    = 'customs'
  )

  ## check dataset id for valid values
  rlang::arg_match(dataset_id, values = names(switch_list))

  ## assign the proper file name to a new variable
  ref_table_id <- switch_list[dataset_id]

  ## attempt to return the data from the environment first
  data <- get(dataset_id, envir = ct_env)

  ## if the dataset is not yet loaded into the environment
  ## read it from disk and save to environment
  if(is.null(data)){
    data <- fs::path_package(paste0('extdata/',ref_table_id,'.rds'),
                             package = 'comtradr') |>
      readr::read_rds()
    assign(dataset_id,data,envir = ct_env)
  }

  if(update & any(dataset_id %in% ct_env$updated)){
    ## if update is true, but dataset_id has already been updated once
    ## only return message
    if (verbose) {
      cli::cli_inform(c("i" = paste0("Already checked for updates for ",
                                     dataset_id,' in this session.')))
    }
    return(data)
  } else if(update){
    ## if update is true and not yet updated in this session inform user that update process is starting
    if (verbose) {
      cli::cli_inform(c("i" = paste0("Attempting to update reference table: ",
                                     dataset_id)))
    }

    ## download new reference table from the UN
    data_new <- ct_download_ref_table(ref_table_id = ref_table_id)

    if(unique(data_new$last_modified)>unique(data$last_modified)){
      ## if the date last modified, returned in the header is newer than the old data
      if (verbose) {
        cli::cli_inform(c("i" = paste0("Updated reference tables ",
                                       dataset_id,
                                       " with new data, last modified on: ",
                                       unique(data_new$last_modified)))) # nolint
      }

      ## write to environment and overwrite old data
      assign(dataset_id,data_new,envir = ct_env)

      ## let environment variable know that dataset has been updated
      ct_env$updated <- c(ct_env$updated,dataset_id)

      return(data_new)
    } else {
      ## if last_modified is not newer, let user know that datasets are up to date.
      if (verbose) {
        cli::cli_inform(c("i" = paste0('No update necessary for table ',
                                       dataset_id,'.')))
      }

      ## save in env variable, that update has been checked in this session
      ct_env$updated <- c(ct_env$updated,dataset_id)

      return(as.data.frame(data))
    }
  } else {
    ## if no update parameter passed on, just return the data read from disk or the env
      return(as.data.frame(data))
    }
  }



#' Downloading the references tables from UN Comtrade
#'
#' @noRd
ct_download_ref_table <- function(ref_table_id) {
  iso_3 <- id <- group <- category <-
    text <- reporterCodeIsoAlpha3 <- entryEffectiveDate <-  NULL
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
    httr2::resp_body_json(simplifyVector = TRUE)

  ## get date of last modification from headers
  last_modified <-
    httr2::resp_header(header = "Last-Modified", resp = response) |>
    stringr::str_extract(pattern = '(\\d{2} [a-zA-Z]+ \\d{4})') |>
    replace_month() |>
    as.Date(format = "%d %m %Y")

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



#' UN Comtrade commodities database query
#'
#' The Comtrade API requires that searches for specific commodities be done
#' using commodity codes. This is a helper function for querying the
#' Comtrade commodity database. It takes as input a vector of
#' commodities or commodity codes. Output is a list or vector of commodity
#' descriptions or codes associated with the input search_terms. For use with
#' the UN Comtrade API, full API docs can be found at
#' \url{https://unstats.un.org/wiki/display/comtrade/}
#'
#' @inheritParams ct_get_data
#'
#' @param search_terms Commodity names or commodity codes, as a char or numeric
#'  vector.
#'
#' @param return_code Logical, if set to FALSE, the function will return a
#'  set of commodity descriptions along with commodity codes (as a single
#'  string for each match found), if set to TRUE it will return only the
#'  commodity codes. Default value is FALSE.
#' @param return_char Logical, if set to FALSE, the function will return the
#'  matches as a named list, if set to TRUE it will return them as a character
#'  vector. Default value is FALSE.
#' @param verbose Logical, if set to TRUE, a warning message will print to
#'  console if any of the elements of input "search_terms" returned no matches
#'  (message will indicate which elements returned no data). Default is TRUE.
#' @param ignore.case logical, to be passed along to arg ignore.case within
#'  \code{\link{grepl}}. Default value is TRUE.
#' @param ... additional args to be passed along to \code{\link{grepl}}.
#'
#' @return A list or character vector of commodity descriptions and/or
#'  commodity codes that are matches with the elements of "search_terms".
#'
#' @details This function uses regular expressions (regex) to find matches
#'  within the commodity DB. This means it will treat as a match any commodity
#'  description that contains the input search term. For more on using regex
#'  within R, see
#'  \url{https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html}
#'
#' @export
#'
#' @seealso \code{\link{grepl}}
#'
#' @examplesIf interactive()
#' comtradr::ct_commodity_lookup('wine')

ct_commodity_lookup <- function(search_terms,
                                return_code = FALSE,
                                commodity_classification = 'HS',
                                type = 'goods',
                                return_char = FALSE,
                                verbose = TRUE,
                                ignore.case = TRUE,
                                update = FALSE,
                                ...) {
  stopifnot(mode(search_terms) %in% c("numeric", "character"))
  search_terms <- as.character(search_terms)

  commodity_classification <- check_clCode(check_type(type),
                                           commodity_classification)

  # Fetch the commodity database from ct_env.
  commodity_df <- ct_get_ref_table(dataset_id = commodity_classification,
                                   update,
                                   verbose)


  # transform input arg "return_code" to match the col name indicated
  # (TRUE == "code", FALSE == "commodity").
  if (return_code) {
    return_col <- "id"
  } else {
    return_col <- "text"
  }

  # For each element of input arg "search_terms", fetch all commodity
  # descriptions and/or codes from the database. Output will be a list.
  ans <- purrr::map(search_terms, function(x) {
    # Determine whether the param 'value' is a commodity or a code, then
    # perform the look up.
    if (grepl("[A-z]", x)) {
      lu <- "text"
    } else {
      lu <- "id"
    }
    commodity_df[grepl(x, commodity_df[[lu]],
                       ignore.case = ignore.case), return_col]
  })

  # If "verbose" == TRUE, create warning message if any of the elements of
  # input arg "search_terms" produced no search results.
  if (verbose) {
    check_len <- purrr::map_int(ans, length)
    if (any(check_len == 0)) {
      if (any(check_len > 0)) {
        msg <- paste0(
          "There were no matching results found for inputs: ",
          paste(search_terms[which(check_len == 0)], collapse = ", ")
        )
      } else {
        msg <- "There were no matching results found"
      }
      cli::cli_warn(msg)
    }
  }

  # If "return_char" == TRUE, unlist obj "ans". Otherwise, assign names to the
  # elements of obj "ans" (names will be taken from input arg "search_terms").
  if (return_char) {
    ans <- unlist(ans, FALSE, FALSE)
  } else {
    names(ans) <- search_terms
  }
  return(ans)
}


replace_month <- function(date_str) {
  months <- c("Jan" = "01", "Feb" = "02", "Mar" = "03", "Apr" = "04", "May" = "05", "Jun" = "06",
              "Jul" = "07", "Aug" = "08", "Sep" = "09", "Oct" = "10", "Nov" = "11", "Dec" = "12")
  stringr::str_replace_all(date_str, months)
}
