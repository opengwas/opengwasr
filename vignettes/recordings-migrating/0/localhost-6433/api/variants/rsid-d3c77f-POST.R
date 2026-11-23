structure(list(method = "POST", url = "http://localhost:6433/api/variants/rsid", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:05:13 GMT", 
        server = "uvicorn", location = "/api/jobs/39d2b021", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "396", 
        `content-type` = "application/json", `x-request-id` = "af557baa"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"39d2b021\",\"status_url\":\"/api/jobs/39d2b021\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:61affd84d983af9ec33d28bb982ebc74c168407098017f151e4c9945f6aeaa3a\",\"query_hash\":\"sha256:61affd84d983af9ec33d28bb982ebc74c168407098017f151e4c9945f6aeaa3a\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 7.9e-05, 
    starttransfer = 0.014065, total = 0.014148), cache = new.env(parent = emptyenv())), class = "httr2_response")
