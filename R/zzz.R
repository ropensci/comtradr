## Set up pkg environment to manage variables related to:
# 1. throttling of API queries and rate limiting.
# 2. Comtrade database of countries, stored as pkg data.
# 3. Comtrade database of commodity descriptions and codes, stored as pkg data.
ct_env <- new.env()

# Variables for rate limiting.
assign(
  "ua",
  paste(
    Sys.info()[["user"]], R.version$version.str, version$platform, sep = ", "
  ), envir = ct_env
)

# Initialize placeholders for package data within ct_env.
# assign("country_df", NULL, envir = ct_env)
# assign("commodity_df", NULL, envir = ct_env)
# assign("commodity_df", NULL, envir = ct_env)


assign('B4', NULL, envir = ct_env)
assign('B5', NULL, envir = ct_env)
assign('EB0', NULL, envir = ct_env)
assign('EB10', NULL, envir = ct_env)
assign('EB10s', NULL, envir = ct_env)
assign('EB', NULL, envir = ct_env)
assign('H0', NULL, envir = ct_env)
assign('H1', NULL, envir = ct_env)
assign('H2', NULL, envir = ct_env)
assign('H3', NULL, envir = ct_env)
assign('H4', NULL, envir = ct_env)
assign('H5', NULL, envir = ct_env)
assign('H6', NULL, envir = ct_env)
assign('HS', NULL, envir = ct_env)
assign('S1', NULL, envir = ct_env)
assign('S2', NULL, envir = ct_env)
assign('S3', NULL, envir = ct_env)
assign('S4', NULL, envir = ct_env)
assign('SS', NULL, envir = ct_env)
assign('reporter', NULL, envir = ct_env)
assign('partner', NULL, envir = ct_env)
assign('list_of_datasets', NULL, envir = ct_env)
assign('reporter', ct_get_ref_table('reporter'), envir = ct_env)
assign('partner', ct_get_ref_table('partner'), envir = ct_env)


# Establish initial credentials for the Comtrade API.
# .onLoad <- function(libname, pkgname) {
#   if (is.null(getOption("comtradr"))) {
#     ct_options <- list(
#       comtrade = list(
#         token = NULL,
#         account_type = "standard",
#         per_hour_limit = 100,
#         per_second_limit = 1
#       )
#     )
#     class(ct_options) <- "comtradr_credentials"
#     options(comtradr = ct_options)
#   }
# }

