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


.onLoad <- function(libname, pkgname) {
  max_size_env <- Sys.getenv("COMTRADR_CACHE_MAX_SIZE")
  max_age_env <- Sys.getenv("COMTRADR_CACHE_MAX_AGE")
  max_n_env <- Sys.getenv("COMTRADR_CACHE_MAX_N")

  max_size <- ifelse(nzchar(max_size_env), eval(parse(text = max_size_env)), 1024 * 1024^2)
  max_age <- ifelse(nzchar(max_age_env), eval(parse(text = max_age_env)), Inf)
  max_n <- ifelse(nzchar(max_n_env), eval(parse(text = max_n_env)), Inf)

  cache <- cachem::cache_disk(dir = rappdirs::user_cache_dir(appname = 'comtradr'),
                              max_size = max_size,
                              max_age = max_age,
                              max_n = max_n)

  ct_perform_request_cache <- memoise::memoise(ct_perform_request, cache = cache)

  assign(x = "ct_perform_request_cache",
         value = ct_perform_request_cache,
         envir = rlang::ns_env("comtradr"))

  assign(x = "cache",
         value = cache,
         envir = rlang::ns_env("comtradr"))
}

# Initialize placeholders for package data within ct_env.
assign("B4", NULL, envir = ct_env)
assign("B5", NULL, envir = ct_env)
assign("EB0", NULL, envir = ct_env)
assign("EB10", NULL, envir = ct_env)
assign("EB10s", NULL, envir = ct_env)
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
assign("customs_code", NULL, envir = ct_env)
assign("flow_direction", NULL, envir = ct_env)
assign("reporter", NULL, envir = ct_env)
assign("partner", NULL, envir = ct_env)
assign("list_of_datasets", NULL, envir = ct_env)
assign("reporter", ct_get_ref_table("reporter"), envir = ct_env)
assign("partner", ct_get_ref_table("partner"), envir = ct_env)
assign("updated", "init", envir = ct_env)
