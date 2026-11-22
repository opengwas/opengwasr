structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 17:55:10 GMT", 
        server = "uvicorn", location = "/api/jobs/71cc388a", 
        `retry-after` = "2", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "6af0d183"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"71cc388a\",\"status_url\":\"/api/jobs/71cc388a\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:ac703c1f742e6b1d8dd60795dd22672db8dfa9050a1e9e18122dfa17f6a6ca10\",\"query_hash\":\"sha256:ac703c1f742e6b1d8dd60795dd22672db8dfa9050a1e9e18122dfa17f6a6ca10\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":1.08,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 7.8e-05, 
    starttransfer = 0.024925, total = 0.02501), cache = new.env(parent = emptyenv())), class = "httr2_response")
