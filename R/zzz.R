## Set up pkg environment to manage variables related to:
# 1. throttling of API queries and rate limiting.
# 2. Comtrade database of countries, stored as pkg data.
# 3. Comtrade database of commodity descriptions and codes, stored as pkg data.
ct_env <- new.env()

# Variables for rate limiting.
ct_env <- new.env()
assign("last_query", NULL, envir = ct_env)
assign("next_hour_reset", NULL, envir = ct_env)
assign("queries_this_hour", 100, envir = ct_env)
assign(
  "ua",
  paste(
    Sys.info()[["user"]], R.version$version.str, version$platform, sep = ", "
  ), envir = ct_env
)

# Initialize placeholders for package data within ct_env.
assign("country_df", NULL, envir = ct_env)
assign("commodity_df", NULL, envir = ct_env)

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
