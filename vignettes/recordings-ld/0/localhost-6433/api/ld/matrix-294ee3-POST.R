structure(list(method = "POST", url = "http://localhost:6433/api/ld/matrix", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:03:45 GMT", 
        server = "uvicorn", location = "/api/jobs/d46fb116", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "7137aba6"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"d46fb116\",\"status_url\":\"/api/jobs/d46fb116\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:9599ff822687acc3854142c70ef4a7b208991c795854b0de4dc1e38222189bb0\",\"query_hash\":\"sha256:9599ff822687acc3854142c70ef4a7b208991c795854b0de4dc1e38222189bb0\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.1e-05, 
    starttransfer = 0.013843, total = 0.014124), cache = new.env(parent = emptyenv())), class = "httr2_response")
