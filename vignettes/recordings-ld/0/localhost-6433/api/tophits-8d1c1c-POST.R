structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 09:54:50 GMT", 
        server = "uvicorn", location = "/api/jobs/eb0f46de", 
        `retry-after` = "3", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "2b58efc8"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"eb0f46de\",\"status_url\":\"/api/jobs/eb0f46de\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:95522674f1da7f05abfc342b6c0466db83d5611439ab21689b6bdb550ad57fd7\",\"query_hash\":\"sha256:95522674f1da7f05abfc342b6c0466db83d5611439ab21689b6bdb550ad57fd7\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 7.8e-05, 
    starttransfer = 0.013961, total = 0.014196), cache = new.env(parent = emptyenv())), class = "httr2_response")
