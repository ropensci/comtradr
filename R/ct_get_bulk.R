#' Get trade data from the UN Comtrade API
#'
#' This function queries the UN Comtrade API to retrieve
#' international trade data.
#' It allows for detailed specification of the query,
#' including the type of data (goods or services),
#' frequency (annual or monthly), commodity classification,
#' flow direction, and more.
#' By providing `everything` for certain parameters,
#' you can query all possible values.
#' The function is opinionated in that it already verifies certain parameters
#' for you and is more than a pure wrapper around the API.
#'
#' @details
#' The UN Comtrade database provides a repository of official international
#' trade statistics and relevant analytical tables.
#' It contains annual trade statistics starting from 1988
#' and monthly trade statistics since 2000 for goods data
#'
#' Parameters that accept `everything` will query all possible values.
#' For example, setting `commodity_code = 'everything'`
#' will retrieve data for all commodity codes.
#' This can be useful for broad queries but may result in large datasets.
#'
#' @param frequency The frequency of returned trade data.
#' Possible values: 'A' for annual data, 'M' for monthly data. Default: 'A'.
#' @param type The type of returned trade data.
#' Possible values: 'goods' for trade in goods,
#' 'services' for trade in services. Default: 'goods'.
#' @param commodity_classification The trade classification scheme.
#' Possible values for goods:
#'  `c('HS','H0','H1','H2','H3','H4','H5','H6')`and
#'  `c(S1','S2','S3','S4','SS','B4','B5')`;
#' for services: `c('EB02','EB10','EB10S','EB')`. Default: 'HS'.
#' @param reporter Reporter ISO3 code(s), `everything` or `all_countries`.
#' See `comtradr::country_codes` or `comtradr::ct_get_ref_table('reporter')`
#' for possible values. `all_countries` returns all countries without aggregates
#' `everything` returns all possible parameters. Default: 'all_countries'.
#' @param start_date The start date of the query.
#' Format: `yyyy` for yearly, `yyyy-mm` for monthly.
#' @param end_date The end date of the query.
#' Format: `yyyy` for yearly, `yyyy-mm` for monthly.
#' Max: 12 years after start date for annual data, one year for monthly data.
#' @param primary_token Your primary UN Comtrade API token.
#' Default: stored token from `comtradr::set_primary_comtrade_key`.
#' @param tidy_cols If TRUE, returns tidy column names.
#' If FALSE, returns raw column names. Default: TRUE.
#' @param verbose If TRUE, sends status updates to the console.
#' If FALSE, runs functions quietly. Default: FALSE.
#' @param update If TRUE, downloads possibly updated reference tables
#' from the UN. Default: FALSE.
#' @param requests_per_second Rate of requests per second executed,
#' usually specified as a fraction, e.g. 10/60 for 10 requests per minute,
#' see `req_throttle()` for details.
#' @param cache A logical value to determine, whether requests should be cached
#' or not. If set to True, `tools::R_user_dir(which = 'cache')` is used
#' to determine the location of the cache. Use the .Renviron file to set the
#' R_USER_CACHE_DIR in order to change this location. Default: False.
#' @param download_bulk_files If TRUE downloads all files that are returned
#' from the Comtrade API as a list for the specified parameters. This can
#' result in large writing and reading operations from your file system.
#' @export
#' @returns A data.frame with trade data or,
#' if `process = FALSE`, a httr2 response object.

ct_get_bulk <- function(type = "goods",
                        frequency = "A",
                        commodity_classification = "HS",
                        reporter = "all_countries",
                        start_date = NULL,
                        end_date = NULL,
                        tidy_cols = TRUE,
                        verbose = FALSE,
                        primary_token = get_primary_comtrade_key(),
                        update = FALSE,
                        requests_per_second = 10 / 60,
                        cache = FALSE,
                        download_bulk_files = TRUE) {
  bulk <- TRUE

  ## compile codes
  params <- ct_check_params(
    type = type,
    frequency = frequency,
    commodity_classification = commodity_classification,
    reporter = reporter,
    start_date = start_date,
    end_date = end_date,
    verbose = verbose,
    update = update,
    extra_params = NULL,
    bulk = bulk
  )

  ## this builds the request to get the list of bulk files to be downloaded
  req <-
    ct_build_request(params,
                     verbose = verbose,
                     primary_token = primary_token,
                     bulk = bulk)

  ## if cache is TRUE this will cache the performed request and download the
  ## list

  if (cache) {
    resp <- ct_perform_request_cache(req,
                                     requests_per_second = requests_per_second,
                                     verbose = verbose, bulk = bulk)
  } else{
    resp <- ct_perform_request(req,
                               requests_per_second = requests_per_second,
                               verbose = verbose, bulk = bulk)
  }

  ## this will parse the response to return necessary parameters for actually
  ## downloading the file list
  reporterCode <- fileSize <- rowKey <- NULL

  parsed_response <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("data")

  if(length(parsed_response)==0){
    cli::cli_abort("Probably no data for this combination of parameters")
  }

  file_hash <- parsed_response |>
    poorman::pull(rowKey)

  reporter_code <- parsed_response |>
    poorman::pull(reporterCode)

  if (download_bulk_files) {

    file_size <- parsed_response |>
      poorman::pull(fileSize) |>
      stringr::str_remove_all(",") |>
      convert_file_size() |>
      sum() |>
      format_file_size()

    if (verbose) {
      cli::cli_progress_step(paste0("Will download files size of: ",
                                     file_size))
      ## the progress bar from map overrides the progress step, so i am
      ## including another line here....
      cli::cli_bullets("") # nolint
    }


    reqs <- purrr::pmap(
      list("reporter_code" = reporter_code,
           "file_hash"=file_hash),
      ct_build_request,
      primary_token = primary_token,
      verbose = verbose,
      .progress = verbose
    )

    if (verbose) {
      cli::cli_progress_step(
        "Performing request, which can take a few seconds, depending on the amount of data queried.") # nolint
      ## the progress bar from map overrides the progress step, so i am
      ## including another line here....
      cli::cli_bullets("") # nolint
    }

    if(cache){
      resps <- purrr::map(reqs,
                          ~ ct_perform_request_cache(
                            .x,
                            requests_per_second = 60 /
                              10,
                            verbose = verbose,
                            bulk = bulk
                          ), .progress = verbose)
    } else {
      resps <- purrr::map(reqs,
                          ~ ct_perform_request(
                            .x,
                            requests_per_second = 60 /
                              10,
                            verbose = verbose,
                            bulk = bulk
                          ), .progress = verbose)
    }

    if (verbose) {
      cli::cli_progress_step("Processing bulk file, this writes to your cache directory.") #nolint
      ## the progress bar from map overrides the progress step, so i am
      ## including another line here....
      cli::cli_bullets("") # nolint

      }

    if (cache) {
      result <- purrr::map_dfr(
        resps,
        ~ ct_process_response_cache(
          .x,
          verbose = verbose,
          tidy_cols = tidy_cols,
          bulk = TRUE
        ),
        .progress = verbose
      )
    } else{
      result <- purrr::map_dfr(
        resps,
        ~ ct_process_response(
          .x,
          verbose = verbose,
          tidy_cols = tidy_cols,
          bulk = TRUE
        ),.progress = verbose
      )
    }
    return(result)

  } else {
    return(parsed_response)
  }

}

