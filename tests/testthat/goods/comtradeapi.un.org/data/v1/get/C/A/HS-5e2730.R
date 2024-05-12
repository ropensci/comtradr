structure(list(method = "GET", url = "https://comtradeapi.un.org/data/v1/get/C/A/HS?cmdCode=TOTAL&partnerCode=32%2C280%2C276&reporterCode=156&period=2010&motCode=0&partner2Code=0&customsCode=C00&includeDesc=TRUE", 
    status_code = 200L, headers = structure(list(`Transfer-Encoding` = "chunked", 
        `Content-Type` = "application/json; charset=utf-8", `Content-Encoding` = "gzip", 
        Vary = "Accept-Encoding", `Request-Context` = "appId=cid-v1:9b6e1d5a-3728-46ff-b743-6d33d23e54a6", 
        `x-frame-options` = "deny", `X-Content-Type-Options` = "nosniff", 
        `X-XSS-Protection` = "1;mode=block", `strict-transport-security` = "max-age=31536000;includeSubDomains", 
        `content-security-policy` = "frame-src 'self'", `x-permitted-cross-domain-policies` = "none", 
        `Referrer-Policy` = "no-referrer-when-downgrade", `Permissions-Policy` = "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()", 
        Date = "Sun, 12 May 2024 16:49:10 GMT"), class = "httr2_headers"), 
    body = charToRaw("{\"elapsedTime\":\"0.14 secs\",\"count\":4,\"data\":[{\"typeCode\":\"C\",\"freqCode\":\"A\",\"refPeriodId\":20100101,\"refYear\":2010,\"refMonth\":52,\"period\":\"2010\",\"reporterCode\":156,\"reporterISO\":\"CHN\",\"reporterDesc\":\"China\",\"flowCode\":\"M\",\"flowDesc\":\"Import\",\"partnerCode\":32,\"partnerISO\":\"ARG\",\"partnerDesc\":\"Argentina\",\"partner2Code\":0,\"partner2ISO\":\"W00\",\"partner2Desc\":\"World\",\"classificationCode\":\"H3\",\"classificationSearchCode\":\"HS\",\"isOriginalClassification\":true,\"cmdCode\":\"TOTAL\",\"cmdDesc\":\"All Commodities\",\"aggrLevel\":0,\"isLeaf\":false,\"customsCode\":\"C00\",\"customsDesc\":\"TOTAL CPC\",\"mosCode\":\"0\",\"motCode\":0,\"motDesc\":\"TOTAL MOT\",\"qtyUnitCode\":-1,\"qtyUnitAbbr\":\"N/A\",\"qty\":null,\"isQtyEstimated\":false,\"altQtyUnitCode\":-1,\"altQtyUnitAbbr\":\"N/A\",\"altQty\":null,\"isAltQtyEstimated\":false,\"netWgt\":null,\"isNetWgtEstimated\":false,\"grossWgt\":null,\"isGrossWgtEstimated\":false,\"cifvalue\":6804128143.0,\"fobvalue\":null,\"primaryValue\":6804128143.0,\"legacyEstimationFlag\":0,\"isReported\":true,\"isAggregate\":false},{\"typeCode\":\"C\",\"freqCode\":\"A\",\"refPeriodId\":20100101,\"refYear\":2010,\"refMonth\":52,\"period\":\"2010\",\"reporterCode\":156,\"reporterISO\":\"CHN\",\"reporterDesc\":\"China\",\"flowCode\":\"X\",\"flowDesc\":\"Export\",\"partnerCode\":32,\"partnerISO\":\"ARG\",\"partnerDesc\":\"Argentina\",\"partner2Code\":0,\"partner2ISO\":\"W00\",\"partner2Desc\":\"World\",\"classificationCode\":\"H3\",\"classificationSearchCode\":\"HS\",\"isOriginalClassification\":true,\"cmdCode\":\"TOTAL\",\"cmdDesc\":\"All Commodities\",\"aggrLevel\":0,\"isLeaf\":false,\"customsCode\":\"C00\",\"customsDesc\":\"TOTAL CPC\",\"mosCode\":\"0\",\"motCode\":0,\"motDesc\":\"TOTAL MOT\",\"qtyUnitCode\":-1,\"qtyUnitAbbr\":\"N/A\",\"qty\":null,\"isQtyEstimated\":false,\"altQtyUnitCode\":-1,\"altQtyUnitAbbr\":\"N/A\",\"altQty\":null,\"isAltQtyEstimated\":false,\"netWgt\":null,\"isNetWgtEstimated\":false,\"grossWgt\":null,\"isGrossWgtEstimated\":false,\"cifvalue\":null,\"fobvalue\":6115764185.0,\"primaryValue\":6115764185.0,\"legacyEstimationFlag\":0,\"isReported\":true,\"isAggregate\":false},{\"typeCode\":\"C\",\"freqCode\":\"A\",\"refPeriodId\":20100101,\"refYear\":2010,\"refMonth\":52,\"period\":\"2010\",\"reporterCode\":156,\"reporterISO\":\"CHN\",\"reporterDesc\":\"China\",\"flowCode\":\"M\",\"flowDesc\":\"Import\",\"partnerCode\":276,\"partnerISO\":\"DEU\",\"partnerDesc\":\"Germany\",\"partner2Code\":0,\"partner2ISO\":\"W00\",\"partner2Desc\":\"World\",\"classificationCode\":\"H3\",\"classificationSearchCode\":\"HS\",\"isOriginalClassification\":true,\"cmdCode\":\"TOTAL\",\"cmdDesc\":\"All Commodities\",\"aggrLevel\":0,\"isLeaf\":false,\"customsCode\":\"C00\",\"customsDesc\":\"TOTAL CPC\",\"mosCode\":\"0\",\"motCode\":0,\"motDesc\":\"TOTAL MOT\",\"qtyUnitCode\":-1,\"qtyUnitAbbr\":\"N/A\",\"qty\":null,\"isQtyEstimated\":false,\"altQtyUnitCode\":-1,\"altQtyUnitAbbr\":\"N/A\",\"altQty\":null,\"isAltQtyEstimated\":false,\"netWgt\":null,\"isNetWgtEstimated\":false,\"grossWgt\":null,\"isGrossWgtEstimated\":false,\"cifvalue\":74251272075.0,\"fobvalue\":null,\"primaryValue\":74251272075.0,\"legacyEstimationFlag\":0,\"isReported\":true,\"isAggregate\":false},{\"typeCode\":\"C\",\"freqCode\":\"A\",\"refPeriodId\":20100101,\"refYear\":2010,\"refMonth\":52,\"period\":\"2010\",\"reporterCode\":156,\"reporterISO\":\"CHN\",\"reporterDesc\":\"China\",\"flowCode\":\"X\",\"flowDesc\":\"Export\",\"partnerCode\":276,\"partnerISO\":\"DEU\",\"partnerDesc\":\"Germany\",\"partner2Code\":0,\"partner2ISO\":\"W00\",\"partner2Desc\":\"World\",\"classificationCode\":\"H3\",\"classificationSearchCode\":\"HS\",\"isOriginalClassification\":true,\"cmdCode\":\"TOTAL\",\"cmdDesc\":\"All Commodities\",\"aggrLevel\":0,\"isLeaf\":false,\"customsCode\":\"C00\",\"customsDesc\":\"TOTAL CPC\",\"mosCode\":\"0\",\"motCode\":0,\"motDesc\":\"TOTAL MOT\",\"qtyUnitCode\":-1,\"qtyUnitAbbr\":\"N/A\",\"qty\":null,\"isQtyEstimated\":false,\"altQtyUnitCode\":-1,\"altQtyUnitAbbr\":\"N/A\",\"altQty\":null,\"isAltQtyEstimated\":false,\"netWgt\":null,\"isNetWgtEstimated\":false,\"grossWgt\":null,\"isGrossWgtEstimated\":false,\"cifvalue\":null,\"fobvalue\":68047133397.0,\"primaryValue\":68047133397.0,\"legacyEstimationFlag\":0,\"isReported\":true,\"isAggregate\":false}],\"error\":\"\"}"), 
    request = structure(list(url = "https://comtradeapi.un.org/data/v1/get/C/A/HS?cmdCode=TOTAL&partnerCode=32%2C280%2C276&reporterCode=156&period=2010&motCode=0&partner2Code=0&customsCode=C00&includeDesc=TRUE", 
        method = NULL, headers = structure(list(`Ocp-Apim-Subscription-Key` = "xxxx"), redact = character(0)), 
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
