structure(list(method = "GET", url = "https://comtradeapi.un.org/data/v1/get/S/A/EB?cmdCode=200&flowCode=M%2CX%2CRM%2CRX&partnerCode=32%2C280%2C276&reporterCode=156&period=2010&motCode=0&partner2Code=0&customsCode=C00&includeDesc=TRUE", 
    status_code = 401L, headers = structure(list(`Content-Length` = "143", 
        `Content-Type` = "application/json", `Request-Context` = "appId=cid-v1:9b6e1d5a-3728-46ff-b743-6d33d23e54a6", 
        `WWW-Authenticate` = "AzureApiManagementKey realm=\"https://comtradeapi.un.org/data\",name=\"Ocp-Apim-Subscription-Key\",type=\"header\"", 
        Date = "Fri, 01 Sep 2023 17:43:39 GMT"), class = "httr2_headers"), 
    body = charToRaw("{ \"statusCode\": 401, \"message\": \"Access denied due to invalid subscription key. Make sure to provide a valid key for an active subscription.\" }")), class = "httr2_response")
