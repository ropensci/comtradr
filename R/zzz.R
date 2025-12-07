## Set up pkg environment to manage variables related to:
# 2. Comtrade database of countries, stored as pkg data.
# 3. Comtrade database of commodity descriptions and codes, stored as pkg data.
ct_env <- new.env()

assign(
  "ua",
  paste(
    Sys.info()[["user"]], R.version$version.str, version$platform,
    sep = ", "
  ),
  envir = ct_env
)


check_old_cache_message <- function() {
  cli::cli_inform(
    c(
      "In the last version of comtradr the cache location has been changed, because it was not CRAN compliant. You can:", #nolint
      'v' = 'Migrate the cache and clean old files with {.run ct_migrate_cache()}', #nolint
      'x' = 'Ignore this warning, a new cache is created automatically.',
      'i' = "Delete your old cache manually with:",
      '*' = "{.run rappdirs::user_cache_dir('comtradr') |> list.files(full.names = T) |>   file.remove()}",  #nolint
      ' ' = "and",
      '*' = "{.run rappdirs::user_cache_dir('comtradr_bulk') |> list.files(full.names = T) |>   file.remove()}"), #nolint
    class = "packageStartupMessage"
  )
}

.onAttach <- function(libname, pkgname) {
  ## throw a warning, if a cache in the non-compliant directory is detected
  ## but only if there is actually any files in this directory
  ## pointing to the migrating function
  if ((
    rappdirs::user_cache_dir('comtradr') !=
    tools::R_user_dir('comtradr', which = 'cache')
  ) &&
  (length(list.files(rappdirs::user_cache_dir('comtradr'))) > 0 |
   length(list.files(
     rappdirs::user_cache_dir('comtradr_bulk')
   )) > 0)) {
    check_old_cache_message()
    }
}



.onLoad <- function(libname, pkgname) {
  max_size_env <- Sys.getenv("COMTRADR_CACHE_MAX_SIZE")
  max_age_env <- Sys.getenv("COMTRADR_CACHE_MAX_AGE")
  max_n_env <- Sys.getenv("COMTRADR_CACHE_MAX_N")

  max_size <- ifelse(nzchar(max_size_env), eval(parse(text = max_size_env)),
                     1024 * 1024^2)
  max_age <- ifelse(nzchar(max_age_env), eval(parse(text = max_age_env)), Inf)
  max_n <- ifelse(nzchar(max_n_env), eval(parse(text = max_n_env)), Inf)

  cache <- cachem::cache_disk(dir = tools::R_user_dir('comtradr',
                                                      which = 'cache'),
                              max_size = max_size,
                              max_age = max_age,
                              max_n = max_n)

  ct_perform_request_cache <- memoise::memoise(ct_perform_request,
                                               cache = cache)

  assign(x = "ct_perform_request_cache",
         value = ct_perform_request_cache,
         envir = rlang::ns_env("comtradr"))

  ct_process_response_cache <- memoise::memoise(ct_process_response,
                                               cache = cache)

  assign(x = "ct_process_response_cache",
         value = ct_process_response_cache,
         envir = rlang::ns_env("comtradr"))

  assign(x = "cache",
         value = cache,
         envir = rlang::ns_env("comtradr"))



}

# Initialize placeholders for package data within ct_env.
assign("B4", NULL, envir = ct_env)
assign("B5", NULL, envir = ct_env)
assign("EB02", NULL, envir = ct_env)
assign("EB10", NULL, envir = ct_env)
assign("EB10S", NULL, envir = ct_env)
assign("EB", NULL, envir = ct_env)
assign("H0", NULL, envir = ct_env)
assign("H1", NULL, envir = ct_env)
assign("H2", NULL, envir = ct_env)
assign("H3", NULL, envir = ct_env)
assign("H4", NULL, envir = ct_env)
assign("H5", NULL, envir = ct_env)
assign("H6", NULL, envir = ct_env)
assign("HS", NULL, envir = ct_env)
assign("S1", NULL, envir = ct_env)
assign("S2", NULL, envir = ct_env)
assign("S3", NULL, envir = ct_env)
assign("S4", NULL, envir = ct_env)
assign("SS", NULL, envir = ct_env)
assign("mode_of_transport", NULL, envir = ct_env)
assign("mode_of_supply", NULL, envir = ct_env)
assign("available_variables", NULL, envir = ct_env)
assign("units_of_quantity", NULL, envir = ct_env)
assign("frequency", NULL, envir = ct_env)
assign("customs_code", NULL, envir = ct_env)
assign("flow_direction", NULL, envir = ct_env)
assign("reporter", NULL, envir = ct_env)
assign("partner", NULL, envir = ct_env)
assign("list_of_datasets", NULL, envir = ct_env)
assign("reporter", ct_get_ref_table("reporter"), envir = ct_env)
assign("partner", ct_get_ref_table("partner"), envir = ct_env)
assign("updated", "init", envir = ct_env)
