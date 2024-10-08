#' Processes the response object
#'
#' The function is internally called by `ct_get_data()` and processes
#' the httr2response object returned by `ct_perform_request()`, it parses the
#' JSON and adds the respective ISO codes for the reporter and partner
#' countries, as well as the commodity code description.
#'
#' @param resp a valid httr2 response object created
#' from the function `ct_perform_request()`
#'
#' @returns a data.frame object with the results
#'
#' @noRd
#' @inheritParams ct_get_data
ct_process_response <-
  function(resp, verbose = FALSE, tidy_cols, bulk) {
    if (bulk) {

      if (!dir.exists(tools::R_user_dir('comtradr_bulk',which = 'cache'))) {
        dir.create(tools::R_user_dir('comtradr_bulk',which = 'cache'),
                   recursive = TRUE)
      }
      filename <- httr2::resp_header(resp, "Content-Disposition") |>
        stringr::str_remove('.*filename="') |>
        stringr::str_remove('".*') |>
        stringr::str_replace_all("/", "_")

      resp |>
        httr2::resp_body_raw() |>
        writeBin(con = file.path(tools::R_user_dir('comtradr_bulk',
                                                   which = 'cache'),
                                 filename))


      processed <- readr::read_delim(file.path(
        tools::R_user_dir('comtradr_bulk',
                          which = 'cache'),
                                               filename),
                                     delim = "\t",
                                     show_col_types = FALSE,progress = FALSE,
                                     guess_max = 99999,
        col_types = readr::cols(.default = "c"))
      file.remove(file.path(tools::R_user_dir('comtradr_bulk',
                                              which = 'cache'),
                            filename))
    } else {
      result <- resp |>
        httr2::resp_body_json(simplifyVector = TRUE)

      if (length(result$data) > 0) {
        if (nrow(result$data) == 100000) {
          cli::cli_warn(
            c("x" = "Your request returns exactly 100k rows. This means that most likely not all the data you queried has been returned, as the upper limit without subscription is 100k. Please partition your API call, e.g. by using only half the period in the first call.") # nolint
          )
        } else if (nrow(result$data) > 90000) {
          cli::cli_inform(
            c("i" = "Your request has passed 90k rows. If you exceed 100k rows Comtrade will not return all data. You will have to slice your request in smaller parts.") # nolint
          ) # nolint
        }

        processed <- result$data

        return(processed)

      } else {
        return(data.frame(count = 0))
      }
    }
    new_cols <- comtradr::ct_pretty_cols
    if (tidy_cols == TRUE) {
      # Input validation and check to make sure the col headers are found in the
      # package data obj ct_pretty_cols.
      stopifnot(is.data.frame(processed))
      curr_cols <- colnames(processed)

      if (!all(curr_cols %in% new_cols$from)) {
        err <- paste(curr_cols[!curr_cols %in% new_cols$from],
                     collapse = ", ")
        rlang::abort(
          paste(
            "The following col headers within input df are not found in",
            # nolint
            "the pkg data obj 'ct_pretty_cols':",
            err
          )
        )
      }

      colnames(processed) <- purrr::map_chr(curr_cols, function(x) {
        new_cols$to[which(new_cols$from == x)]
      })
    attributes(processed)$url <- resp$url
    attributes(processed)$time <- Sys.time()
    return(processed)
  }
}
