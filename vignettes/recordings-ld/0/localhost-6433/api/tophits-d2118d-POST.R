structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:04:22 GMT", 
        server = "uvicorn", location = "/api/jobs/01cf3957", 
        `retry-after` = "3", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "b21a3113"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"01cf3957\",\"status_url\":\"/api/jobs/01cf3957\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:5e5f4def7d9ef467d69a086f7fe0ba8e394685365dd08fb76e2e5d8cc7edd0bb\",\"query_hash\":\"sha256:5e5f4def7d9ef467d69a086f7fe0ba8e394685365dd08fb76e2e5d8cc7edd0bb\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 7.2e-05, 
    starttransfer = 0.015995, total = 0.016055), cache = new.env(parent = emptyenv())), class = "httr2_response")
