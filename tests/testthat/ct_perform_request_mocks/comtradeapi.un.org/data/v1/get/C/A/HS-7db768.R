structure(list(method = "GET", url = "https://comtradeapi.un.org/data/v1/get/C/A/HS?cmdCode=TOTAL&reporterCode=156&period=2020", 
    status_code = 401L, headers = structure(list(`Content-Length` = "152", 
        `Content-Type` = "application/json", `Request-Context` = "appId=cid-v1:9b6e1d5a-3728-46ff-b743-6d33d23e54a6", 
        `WWW-Authenticate` = "AzureApiManagementKey realm=\"https://comtradeapi.un.org/data\",name=\"Ocp-Apim-Subscription-Key\",type=\"header\"", 
        Date = "Sun, 07 Dec 2025 14:59:17 GMT"), class = "httr2_headers"), 
    body = charToRaw("{ \"statusCode\": 401, \"message\": \"Access denied due to missing subscription key. Make sure to include subscription key when making requests to an API.\" }"), 
    timing = c(redirect = 0, namelookup = 0.35861, connect = 0.574862, 
    pretransfer = 1.030298, starttransfer = 1.2588, total = 1.258888
    ), cache = new.env(parent = emptyenv())), class = "httr2_response")
