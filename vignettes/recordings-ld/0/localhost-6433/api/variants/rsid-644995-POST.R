structure(list(method = "POST", url = "http://localhost:6433/api/variants/rsid", 
    status_code = 202L, headers = structure(list(date = "Tue, 24 Nov 2026 09:08:52 GMT", 
        server = "uvicorn", location = "/api/jobs/52475784", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "803eaaca"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"52475784\",\"status_url\":\"/api/jobs/52475784\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:57a7ed83d731b9b175adfa630bf730b175ef116f237dca92a69e12fbae2a23bc\",\"query_hash\":\"sha256:57a7ed83d731b9b175adfa630bf730b175ef116f237dca92a69e12fbae2a23bc\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000133, 
    starttransfer = 0.009535, total = 0.009629), cache = new.env(parent = emptyenv())), class = "httr2_response")
