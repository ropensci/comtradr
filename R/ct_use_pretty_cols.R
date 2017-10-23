#' Use Pretty Column Headers
#'
#' Transform the column headers of return data from function
#' \code{\link{ct_search}} into a more "polished" set of column headers.
#' Intended for use with plots, publication tables, etc.
#'
#' @param df data frame, Comtrade API data frame, returned from function
#'  \code{\link{ct_search}}.
#'
#' @return data frame, input df with polish column headers.
#' @export
#'
#' @examples \dontrun{
#' # Pull API data
#' df <- ct_search("Germany", "Canada")
#'
#' # Use polished column names
#' df <- ct_use_pretty_cols(df)
#' }
ct_use_pretty_cols <- function(df) {
  # Load package data "ct_pretty_cols".
  new_cols <- comtradr::ct_pretty_cols

  # Input validation and check to make sure the col headers are found in the
  # package data obj ct_pretty_cols.
  stopifnot(is.data.frame(df))
  curr_cols <- colnames(df)
  if (all(curr_cols %in% names(new_cols))) {
    return(df)
  }
  if (!all(curr_cols %in% new_cols)) {
    err <- paste(
      curr_cols[!curr_cols %in% new_cols],
      collapse = ", "
    )
    stop(paste("The following col headers within input df are not found in",
               "the pkg data obj 'ct_pretty_cols':", err))
  }

  # Map col headers of input df to the pkg obj ct_pretty_cols.
  colnames(df) <- purrr::map_chr(curr_cols, function(x) {
    names(new_cols)[which(new_cols == x)]
  })

  return(df)
}
