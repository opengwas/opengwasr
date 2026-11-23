structure(list(method = "POST", url = "http://localhost:6433/api/ld/matrix", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:03:45 GMT", 
        server = "uvicorn", location = "/api/jobs/7d191871", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "9da5ae1d"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"7d191871\",\"status_url\":\"/api/jobs/7d191871\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:39d5a40d915c34ae57a5166e53c8823b5f397db90eaa8a29865a45a6b6bed156\",\"query_hash\":\"sha256:39d5a40d915c34ae57a5166e53c8823b5f397db90eaa8a29865a45a6b6bed156\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 7.9e-05, 
    starttransfer = 0.014087, total = 0.01417), cache = new.env(parent = emptyenv())), class = "httr2_response")
