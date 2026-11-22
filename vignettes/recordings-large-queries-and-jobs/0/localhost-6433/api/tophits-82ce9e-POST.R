structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:15:05 GMT", 
        server = "uvicorn", location = "/api/jobs/065b76b9", 
        `retry-after` = "7", `x-ogdb-cost-estimate` = "5", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "16587339"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"065b76b9\",\"status_url\":\"/api/jobs/065b76b9\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:d58ba1656a4726580839a1fa4d612deed83311db4321967bc83d0b5463e22568\",\"query_hash\":\"sha256:d58ba1656a4726580839a1fa4d612deed83311db4321967bc83d0b5463e22568\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.6e-05, 
    starttransfer = 0.013396, total = 0.013549), cache = new.env(parent = emptyenv())), class = "httr2_response")
