structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:15:05 GMT", 
        server = "uvicorn", location = "/api/jobs/8b0cf1e4", 
        `retry-after` = "7", `x-ogdb-cost-estimate` = "5", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "41ead302"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"8b0cf1e4\",\"status_url\":\"/api/jobs/8b0cf1e4\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:0ecc561bfb772f6b74cbf8978f939eb90e6f3b3bb9ee79654a40b31b7dd25db2\",\"query_hash\":\"sha256:0ecc561bfb772f6b74cbf8978f939eb90e6f3b3bb9ee79654a40b31b7dd25db2\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000149, 
    starttransfer = 0.011472, total = 0.011665), cache = new.env(parent = emptyenv())), class = "httr2_response")
