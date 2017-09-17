# Set up pkg environment to manage variables related to:
# 1. throttling of API queries.
# 2. Comtrade database of countries.
# 3. Comtrade database of commodity descriptions and codes.
ct_env <- new.env()
assign("last_query", NULL, envir = ct_env)
assign("next_hour_reset", NULL, envir = ct_env)
assign("queries_this_hour", 100, envir = ct_env)
load(
  system.file("extdata", "country_table.rda", package = "comtradr"),
  envir = ct_env
)
load(
  system.file("extdata", "commodity_table.rda", package = "comtradr"),
  envir = ct_env
)


# Establish initial credentials for the Comtrade API.
.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("comtradr"))) {
    ct_options <- list(
      comtrade = list(
        token = NULL,
        account_type = "standard",
        per_hour_limit = 100,
        per_second_limit = 1
      )
    )
    class(ct_options) <- "comtradr_credentials"
    options(comtradr = ct_options)
  }
}
