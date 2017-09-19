## Set up pkg environment to manage variables related to:
## 1. throttling of API queries.
## 2. Comtrade database of countries.
## 3. Comtrade database of commodity descriptions and codes.
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
# Read in country DB and commodity DB. If either file doesn't exist, assign
# the DB variable as NULL within env ct_env.
country_file <- system.file("extdata",
                            "country_table.rda",
                            package = "comtradr")
commodity_file <- system.file("extdata",
                              "commodity_table.rda",
                              package = "comtradr")

if (!file.exists(country_file)) {
  assign("country_df", NULL, envir = ct_env)
} else {
  load(country_file, envir = ct_env)
}

if (!file.exists(commodity_file)) {
  assign("commodity_df", NULL, envir = ct_env)
} else {
  load(commodity_file, envir = ct_env)
}


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
