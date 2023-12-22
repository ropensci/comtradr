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
