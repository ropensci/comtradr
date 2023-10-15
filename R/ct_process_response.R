#' Processes the response object
#'
#' The function processes the httr2response object, parses the json and adds the respective iso codes for the reporter and partner countries, as well as the commodity code description.
#'
#' @param resp a valid httr2 response object created from the function `ct_perform_request()`
#' @param verbose whether the function sends status updates to the console
#'
#' @inheritParams ct_get_data
#' @returns a data.frame object with the results
#'
#' @examplesIf interactive()
#' ct_get_data(commodity_code = NULL,
#'             reporter = 'CHN',
#'             partner = 'DEU',
#'             start_date = '2019',
#'             end_date = '2019',
#'             flow_direction = 'import')
#' req <- httr2::last_request()
#' resp <- ct_perform_request(req, requests_per_second = 10/60, verbose = FALSE)
#' data <- ct_process_response(resp)
ct_process_response <- function(resp, verbose = FALSE, tidy_cols) {
  result <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE)



  if (length(result$data) > 0) {
    if(nrow(result$data)==250000){
      cli::cli_warn(c("x" ='Your request returns exactly 250k rows. This means that most likely not all the data you queried has been returned, as the upper limit is 250k. Please partition your API call, e.g. by using only half the period in the first call.'))
    } else if(nrow(result$data)>200000){
      cli::cli_inform(c("i" = "Your request has passed 200k rows. If you exceed 250k rows Comtrade will not return all data. You will have to slice your request in smaller parts."))
    }

    processed <- result$data

    new_cols <- comtradr::ct_pretty_cols
    if(tidy_cols == TRUE){

      # Input validation and check to make sure the col headers are found in the
      # package data obj ct_pretty_cols.
      stopifnot(is.data.frame(processed))
      curr_cols <- colnames(processed)

      if (!all(curr_cols %in% new_cols$from)) {
        err <- paste(
          curr_cols[!curr_cols %in% new_cols$from],
          collapse = ", "
        )
        rlang::abort(paste("The following col headers within input df are not found in",
                   "the pkg data obj 'ct_pretty_cols':", err))
      }

      colnames(processed) <- purrr::map_chr(curr_cols, function(x) {
        new_cols$to[which(new_cols$from == x)]
      })

    }
    attributes(processed)$url <- resp$url
    attributes(processed)$time <- Sys.time()
    return(processed)
  } else {
    return(data.frame(count = 0))
  }
}
