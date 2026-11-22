structure(list(method = "POST", url = "http://localhost:6433/api/variants/chrpos", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 22:58:34 GMT", 
        server = "uvicorn", location = "/api/jobs/27e5e0e5", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "ec8fd584"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"27e5e0e5\",\"status_url\":\"/api/jobs/27e5e0e5\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:6f848898d3fcf8ee78e2ddcaa840e0585023f7270a2a0ca15e4cb131fcb3b28f\",\"query_hash\":\"sha256:6f848898d3fcf8ee78e2ddcaa840e0585023f7270a2a0ca15e4cb131fcb3b28f\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000127, 
    starttransfer = 0.019535, total = 0.019636), cache = new.env(parent = emptyenv())), class = "httr2_response")
