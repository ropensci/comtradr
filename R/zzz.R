# Set up pkg environment to manage throttling of API queries.
ct_limit_cache <- new.env()
assign("last_query", Sys.time(), envir = ct_limit_cache)
assign("next_hour_reset", NULL, envir = ct_limit_cache)
assign("queries_this_hour", 100, envir = ct_limit_cache)

# Establish initial credentials for the Comtrade API.
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("comtradr"))) {
    ct_options <- list(
      comtrade = list(
        token = NA,
        account_type = "standard",
        per_hour_limit = 100,
        per_second_limit = 1
      )
    )
    class(ct_options) <- "comtradr_credentials"
    options(comtradr = ct_options)
  }
}
