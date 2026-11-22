structure(list(method = "POST", url = "http://localhost:6433/api/ld/proxies", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 22:58:45 GMT", 
        server = "uvicorn", location = "/api/jobs/4a6a4471", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "871bdb7d"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"4a6a4471\",\"status_url\":\"/api/jobs/4a6a4471\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:198ee1763a8101d9eebe89f3722f8b8ae1a2d16613794049311d3eda6a37063c\",\"query_hash\":\"sha256:198ee1763a8101d9eebe89f3722f8b8ae1a2d16613794049311d3eda6a37063c\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.6e-05, 
    starttransfer = 0.016328, total = 0.016592), cache = new.env(parent = emptyenv())), class = "httr2_response")
