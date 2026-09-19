structure(list(method = "GET", url = "https://comtradeapi.un.org/data/v1/getTradeMatrix/C/A/TM?cmdCode=TOTAL&flowCode=X&partnerCode=251%2C250&reporterCode=280%2C276&period=2023&includeDesc=TRUE", 
    status_code = 200L, headers = structure(list(`Transfer-Encoding` = "chunked", 
        `Content-Type` = "application/json; charset=utf-8", `Content-Encoding` = "gzip", 
        Vary = "Accept-Encoding", `Request-Context` = "appId=cid-v1:9b6e1d5a-3728-46ff-b743-6d33d23e54a6", 
        `x-frame-options` = "deny", `X-Content-Type-Options` = "nosniff", 
        `X-XSS-Protection` = "1;mode=block", `strict-transport-security` = "max-age=31536000;includeSubDomains", 
        `content-security-policy` = "frame-src 'self'", `x-permitted-cross-domain-policies` = "none", 
        `Referrer-Policy` = "no-referrer-when-downgrade", `Permissions-Policy` = "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()", 
        Date = "Thu, 30 Jul 2026 22:53:40 GMT"), class = "httr2_headers"), 
    body = charToRaw("{\"elapsedTime\":\"0.34 secs\",\"count\":1,\"data\":[{\"typeCode\":\"C\",\"freqCode\":\"A\",\"refPeriodId\":20230101,\"refYear\":2023,\"refMonth\":52,\"period\":\"2023\",\"reporterCode\":276,\"reporterISO\":\"DEU\",\"reporterDesc\":\"Germany\",\"flowCode\":\"X\",\"flowDesc\":\"Export\",\"partnerCode\":251,\"partnerISO\":\"FRA\",\"partnerDesc\":\"France\",\"partner2Code\":0,\"partner2ISO\":\"W00\",\"partner2Desc\":\"World\",\"classificationCode\":\"SS\",\"classificationSearchCode\":\"SS\",\"isOriginalClassification\":false,\"cmdCode\":\"TOTAL\",\"cmdDesc\":\"All Commodities\",\"aggrLevel\":0,\"isLeaf\":false,\"customsCode\":\"C00\",\"customsDesc\":\"TOTAL CPC\",\"mosCode\":\"0\",\"motCode\":0,\"motDesc\":\"TOTAL MOT\",\"qtyUnitCode\":-1,\"qtyUnitAbbr\":\"N/A\",\"qty\":null,\"isQtyEstimated\":false,\"altQtyUnitCode\":-1,\"altQtyUnitAbbr\":\"N/A\",\"altQty\":null,\"isAltQtyEstimated\":false,\"netWgt\":null,\"isNetWgtEstimated\":false,\"grossWgt\":null,\"isGrossWgtEstimated\":false,\"cifvalue\":null,\"fobvalue\":null,\"primaryValue\":130336161906.531,\"legacyEstimationFlag\":0,\"isReported\":false,\"isAggregate\":false}],\"error\":\"\"}"), 
    timing = c(redirect = 0, namelookup = 2.135128, connect = 2.168313, 
    pretransfer = 2.23777, starttransfer = 3.156411, total = 3.156554
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
