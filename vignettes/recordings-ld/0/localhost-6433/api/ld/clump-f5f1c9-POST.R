structure(list(method = "POST", url = "http://localhost:6433/api/ld/clump", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:04:38 GMT", 
        server = "uvicorn", location = "/api/jobs/489e5666", 
        `retry-after` = "2", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "5b7a8557"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"489e5666\",\"status_url\":\"/api/jobs/489e5666\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:28501a880073c0bd0d6ebe33c2413774ef7b4b9121c97b97643dc10c37ab0226\",\"query_hash\":\"sha256:28501a880073c0bd0d6ebe33c2413774ef7b4b9121c97b97643dc10c37ab0226\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000103, 
    starttransfer = 0.018966, total = 0.019221), cache = new.env(parent = emptyenv())), class = "httr2_response")
