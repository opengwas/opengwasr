structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 400L, headers = structure(list(date = "Sun, 22 Nov 2026 20:41:16 GMT", 
        server = "uvicorn", `content-length` = "366", `content-type` = "application/json", 
        `x-request-id` = "089473db-5ada-477c-82df-4a37b2bdb801"), class = "httr2_headers"), 
    body = charToRaw("{\"error\":\"unsupported_threshold\",\"message\":\"threshold must be at most 0.0005, the loosest p-value the Top-Hit Index of OGS-00011 holds\",\"indexed_thresholds\":[5e-08,5e-06,0.0005],\"detail\":{\"error\":\"unsupported_threshold\",\"message\":\"threshold must be at most 0.0005, the loosest p-value the Top-Hit Index of OGS-00011 holds\",\"indexed_thresholds\":[5e-08,5e-06,0.0005]}}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000121, 
    starttransfer = 0.005741, total = 0.005762), cache = new.env(parent = emptyenv())), class = "httr2_response")
