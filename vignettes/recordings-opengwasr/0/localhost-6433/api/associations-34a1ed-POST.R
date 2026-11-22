structure(list(method = "POST", url = "http://localhost:6433/api/associations", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:03:06 GMT", 
        server = "uvicorn", location = "/api/jobs/64e7939d", 
        `retry-after` = "3", `x-ogdb-cost-estimate` = "2", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "b38b38b5"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"64e7939d\",\"status_url\":\"/api/jobs/64e7939d\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:c758966346f36be5149156b72c1db269dcf60a1cba198d7fb397a5caa69795f0\",\"query_hash\":\"sha256:c758966346f36be5149156b72c1db269dcf60a1cba198d7fb397a5caa69795f0\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 7.7e-05, 
    starttransfer = 0.013807, total = 0.014056), cache = new.env(parent = emptyenv())), class = "httr2_response")
