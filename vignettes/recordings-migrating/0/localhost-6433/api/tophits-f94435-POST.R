structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:04:48 GMT", 
        server = "uvicorn", location = "/api/jobs/58f43045", 
        `retry-after` = "4", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "f748e247"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"58f43045\",\"status_url\":\"/api/jobs/58f43045\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:7c3193bde7929f3abc8ed1491005e5ca36a391418034e76d7b041b2e0f6c5ef8\",\"query_hash\":\"sha256:7c3193bde7929f3abc8ed1491005e5ca36a391418034e76d7b041b2e0f6c5ef8\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.3e-05, 
    starttransfer = 0.012433, total = 0.012706), cache = new.env(parent = emptyenv())), class = "httr2_response")
