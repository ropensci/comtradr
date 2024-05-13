structure(list(method = "GET", url = "https://comtradeapi.un.org/bulk/v1/file/32/b07f98ee4cfb477c5b86923bbf159d48ca519ccb35c35865b3578b075fc918fb?format=json", 
    status_code = 200L, headers = structure(list(`Content-Length` = "187742", 
        `Content-Type` = "application/TSV", `Request-Context` = "appId=cid-v1:9b6e1d5a-3728-46ff-b743-6d33d23e54a6", 
        `Content-Disposition` = "attachment; filename=\"C/A/32/C_A_32_196201_S1_O.txt.gz\"; size=187742", 
        `x-frame-options` = "deny", `X-Content-Type-Options` = "nosniff", 
        `X-XSS-Protection` = "1;mode=block", `strict-transport-security` = "max-age=31536000;includeSubDomains", 
        `content-security-policy` = "frame-src 'self'", `x-permitted-cross-domain-policies` = "none", 
        `Referrer-Policy` = "no-referrer-when-downgrade", `Permissions-Policy` = "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()", 
        Date = "Sun, 12 May 2024 16:55:24 GMT"), class = "httr2_headers"), 
    body = as.raw(c(0x1f, 0x8b, 0x08, 0x08, 0x56, 0xd3, 0x22, 
    0x5f, 0x02, 0xff, 0x43, 0x5f, 0x41, 0x5f, 0x33, 0x32, 0x5f, 
    0x31, 0x39, 0x36, 0x32, 0x30, 0x31, 0x5f, 0x53, 0x31, 0x2e, 
    0x74, 0x78, 0x74, 0x00)), request = structure(list(
        url = "https://comtradeapi.un.org/bulk/v1/file/32/b07f98ee4cfb477c5b86923bbf159d48ca519ccb35c35865b3578b075fc918fb?format=json", 
        method = NULL, headers = structure(list(`Ocp-Apim-Subscription-Key` = "xxx"), redact = character(0)), 
        body = NULL, fields = list(), options = list(), policies = list(
            error_body = function (resp) 
            {
                if (!is.null(httr2::resp_header(resp, "Content-Type"))) {
                  if (stringr::str_detect(httr2::resp_header(resp, 
                    "Content-Type"), "json")) {
                    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
                    message <- body$errorObject$errorMessage
                    if (!is.null(message)) {
                      message <- c(message)
                    }
                    return(message)
                  }
                  else if (stringr::str_detect(httr2::resp_header(resp, 
                    "Content-Type"), "text")) {
                    body <- httr2::resp_body_string(resp)
                    if (stringr::str_detect(body, "Request URL Too Long")) {
                      message <- c("You might have provided too many parameters and the URL got too long.")
                      return(message)
                    }
                    else if (stringr::str_detect(body, "The resource you are looking for has been removed")) {
                      message <- c("The original message is: ", 
                        body, "But most likely you have exceeded the character value for the api.")
                      return(message)
                    }
                  }
                }
            }, throttle_delay = function (req) 
            {
                realm <- realm %||% url_parse(req$url)$hostname
                last <- the$throttle[[realm]]
                if (is.null(last)) {
                  wait <- 0
                }
                else {
                  wait <- delay - (unix_time() - last)
                }
                sys_sleep(wait, "for throttling delay")
                throttle_touch(realm)
                wait
            }, retry_max_tries = 2, retry_is_transient = function (resp) 
            {
                (httr2::resp_status(resp) == 403 && httr2::resp_header(resp, 
                  "Retry-After") != "0") || (httr2::resp_status(resp) == 
                  429 && httr2::resp_header(resp, "Retry-After") != 
                  "0")
            }, retry_after = function (resp) 
            {
                time <- as.numeric(httr2::resp_header(resp, "Retry-After"))
                time
            })), class = "httr2_request"), cache = new.env(parent = emptyenv())), class = "httr2_response")
