structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 400L, headers = structure(list(date = "Mon, 23 Nov 2026 09:54:45 GMT", 
        server = "uvicorn", `content-length` = "366", `content-type` = "application/json", 
        `x-request-id` = "b55b8af9"), class = "httr2_headers"), 
    body = charToRaw("{\"error\":\"unsupported_threshold\",\"message\":\"threshold must be at most 0.0005, the loosest p-value the Top-Hit Index of OGS-00011 holds\",\"indexed_thresholds\":[5e-08,5e-06,0.0005],\"detail\":{\"error\":\"unsupported_threshold\",\"message\":\"threshold must be at most 0.0005, the loosest p-value the Top-Hit Index of OGS-00011 holds\",\"indexed_thresholds\":[5e-08,5e-06,0.0005]}}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.8e-05, 
    starttransfer = 0.005042, total = 0.005195), cache = new.env(parent = emptyenv())), class = "httr2_response")
