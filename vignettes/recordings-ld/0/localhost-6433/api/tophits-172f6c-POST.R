structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 09:55:02 GMT", 
        server = "uvicorn", location = "/api/jobs/c62f6cf1", 
        `retry-after` = "2", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "0f36de91"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"c62f6cf1\",\"status_url\":\"/api/jobs/c62f6cf1\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:9437da06dc7f791c2618d0e19811765e5aa693f17408eb910641d00379de9401\",\"query_hash\":\"sha256:9437da06dc7f791c2618d0e19811765e5aa693f17408eb910641d00379de9401\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8e-05, 
    starttransfer = 0.014708, total = 0.014992), cache = new.env(parent = emptyenv())), class = "httr2_response")
