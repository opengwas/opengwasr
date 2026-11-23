structure(list(method = "POST", url = "http://localhost:6433/api/associations", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:05:11 GMT", 
        server = "uvicorn", location = "/api/jobs/31ce40ea", 
        `retry-after` = "2", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "a605ff8a"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"31ce40ea\",\"status_url\":\"/api/jobs/31ce40ea\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:1333e03073edde82420fffe3e6e52dae68d605a719fcee23fb6ae790c423b451\",\"query_hash\":\"sha256:1333e03073edde82420fffe3e6e52dae68d605a719fcee23fb6ae790c423b451\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.2e-05, 
    starttransfer = 0.015688, total = 0.015819), cache = new.env(parent = emptyenv())), class = "httr2_response")
