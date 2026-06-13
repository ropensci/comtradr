#' Build a valid request object from the checked parameters
#'
#' This is an internal function takes the necessary parameters
#' from `ct_check_params()` and creates a httr2 request to be performed.
#' This request can then be used in a second function, `ct_perform_request()`
#' to actually return the data. It is called internally ct `ct_get_data()` and
#' `ct_get_bulk`.
#'
#' @param params a named vector of parameters for the comtrade request,
#' result from `ct_check_params()`.
#'
#' @noRd
#' @returns a httr2 request object
#' @inheritParams ct_get_data
ct_build_request <- function(params,
                             primary_token = NULL,
                             file_hash = NA,
                             reporter_code = NA,
                             verbose = FALSE,
                             bulk) {
  ## if the file_hash is missing we know that this is about the standard API
  if(is.na(file_hash)){
    query_params <- params$query_params

    extra_params <- params$extra_params |>
      purrr::map(unlist) |>
      purrr::pluck(1) |>
      as.list()
    query_params <- c(query_params, extra_params)
    type <- params$url_params$type

    freq <- params$url_params$freq

    clCode <- params$url_params$clCode

    if(bulk){
      base_url <- "https://comtradeapi.un.org/bulk/v1/get/"
    } else {
      base_url <- "https://comtradeapi.un.org/data/v1/get/"
    }

    req <-
      httr2::request(base_url) |>
      httr2::req_url_path_append(type) |>
      httr2::req_url_path_append(freq) |>
      httr2::req_url_path_append(clCode) |>
      httr2::req_headers(`Ocp-Apim-Subscription-Key` = primary_token) |>
      httr2::req_url_query(!!!query_params)

    if (stringr::str_length(req$url) > 2048) {
      rlang::abort("Your request URL exceeds 2048 characters, the upper limit of the Comtrade API. Reduce the number of parameters (e.g. commodity codes) or use `ct_get_bulk()`.") # nolint
    }

    if (verbose) {
      cli::cli_inform(c("i" = paste0("URL that will be queried: ", req$url)))
    }

  } else {
    ## in this case here we are constructing a special request for a bulk file
    req <- httr2::request("https://comtradeapi.un.org/bulk/v1/file/") |>
      httr2::req_url_path_append(list(reporter_code, file_hash)) |>
      httr2::req_url_query(format = "json") |>
      httr2::req_headers(`Ocp-Apim-Subscription-Key` = primary_token)

    if (stringr::str_length(req$url) > 2048) {
      rlang::abort("Your request URL exceeds 2048 characters, the upper limit of the Comtrade API. Reduce the number of parameters (e.g. commodity codes) or use `ct_get_bulk()`.") # nolint
    }
  }


  return(req)
}

#' Split request parameters into batches with URL-size-safe code lists
#'
#' The Comtrade API rejects requests whose query string exceeds about 2048
#' characters with HTTP 414, well below the documented 4096-character URL
#' limit (see issue #103). This helper checks the length the final URL would
#' have and, if it is too long, splits the `partnerCode` and `reporterCode`
#' values into chunks. It returns a list of params objects, one per request;
#' in the common case of a short URL the list contains the unchanged params.
#'
#' @param params checked parameters, result of `ct_check_params()`.
#' @param max_url_chars maximum length of the request URL.
#'
#' @noRd
#' @returns a list of params objects
#' @inheritParams ct_get_data
ct_split_params <- function(params,
                            primary_token = NULL,
                            max_url_chars = 2048L) {
  partner <- params$query_params$partnerCode
  reporter <- params$query_params$reporterCode

  ## length of a code string in the URL: commas are encoded as %2C (+2 chars)
  encoded_length <- function(x) {
    if (is.null(x)) {
      return(0L)
    }
    nchar(x) + 2L * stringr::str_count(x, ",")
  }

  ## URL length with empty partner/reporter values; this request is always
  ## short, so building it never trips the length check in ct_build_request()
  base_params <- params
  base_params$query_params$partnerCode <- ""
  base_params$query_params$reporterCode <- ""
  base_req <- ct_build_request(base_params,
    primary_token = primary_token,
    bulk = FALSE
  )

  budget <- max_url_chars - nchar(base_req$url)

  if (encoded_length(partner) + encoded_length(reporter) <= budget ||
    budget <= 0L) {
    ## fits as is, or too long even without partner/reporter codes, in which
    ## case splitting them cannot help and the request proceeds unchanged
    return(list(params))
  }

  ## a parameter that fits into half the budget is kept whole,
  ## the other parameter gets the remaining budget
  budget_reporter <- min(encoded_length(reporter), budget %/% 2L)
  budget_partner <- budget - budget_reporter

  partner_chunks <- split_codes(partner, budget_partner)
  reporter_chunks <- split_codes(reporter, budget_reporter)

  params_list <- list()
  for (reporter_chunk in reporter_chunks) {
    for (partner_chunk in partner_chunks) {
      chunk_params <- params
      chunk_params$query_params$partnerCode <- partner_chunk
      chunk_params$query_params$reporterCode <- reporter_chunk
      params_list <- c(params_list, list(chunk_params))
    }
  }

  return(params_list)
}

#' Split a comma-separated code string into URL-budget-sized chunks
#'
#' Greedily packs codes into comma-separated strings whose URL-encoded length
#' (commas count as 3 characters, %2C) stays within `budget`. Every chunk
#' contains at least one code, so a single over-long code never produces an
#' empty chunk.
#'
#' @param codes a comma-separated string of codes, or NULL.
#' @param budget maximum URL-encoded length per chunk.
#'
#' @noRd
#' @returns a list of comma-separated code strings (list(NULL) if codes is
#' NULL)
split_codes <- function(codes, budget) {
  if (is.null(codes)) {
    return(list(NULL))
  }
  ids <- strsplit(codes, ",", fixed = TRUE)[[1]]
  chunks <- list()
  current <- character()
  current_length <- 0L
  for (id in ids) {
    addition <- nchar(id) + if (length(current) == 0L) 0L else 3L
    if (length(current) > 0L && current_length + addition > budget) {
      chunks <- c(chunks, list(paste(current, collapse = ",")))
      current <- id
      current_length <- nchar(id)
    } else {
      current <- c(current, id)
      current_length <- current_length + addition
    }
  }
  c(chunks, list(paste(current, collapse = ",")))
}
