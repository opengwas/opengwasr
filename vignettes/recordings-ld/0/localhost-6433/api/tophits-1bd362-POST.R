structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:04:40 GMT", 
        server = "uvicorn", location = "/api/jobs/25ec60b5", 
        `retry-after` = "4", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "b580dc19"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"25ec60b5\",\"status_url\":\"/api/jobs/25ec60b5\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:9e7baa08fad23ae0249813b0209d49902a7752dd72e9c0a2c77385b49fbb725c\",\"query_hash\":\"sha256:9e7baa08fad23ae0249813b0209d49902a7752dd72e9c0a2c77385b49fbb725c\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.4e-05, 
    starttransfer = 0.01313, total = 0.013364), cache = new.env(parent = emptyenv())), class = "httr2_response")
