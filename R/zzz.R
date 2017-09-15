# Set up pkg environment to manage throttling of API queries.
ct_limit_cache <- new.env()
assign("last_query", Sys.time(), envir = ct_limit_cache)
