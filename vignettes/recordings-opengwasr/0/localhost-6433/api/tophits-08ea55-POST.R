structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:03:11 GMT", 
        server = "uvicorn", location = "/api/jobs/ea07a1b2", 
        `retry-after` = "5", `x-ogdb-cost-estimate` = "5", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "16a05e02"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"ea07a1b2\",\"status_url\":\"/api/jobs/ea07a1b2\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:ac703c1f742e6b1d8dd60795dd22672db8dfa9050a1e9e18122dfa17f6a6ca10\",\"query_hash\":\"sha256:ac703c1f742e6b1d8dd60795dd22672db8dfa9050a1e9e18122dfa17f6a6ca10\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8e-05, 
    starttransfer = 0.014652, total = 0.014986), cache = new.env(parent = emptyenv())), class = "httr2_response")
