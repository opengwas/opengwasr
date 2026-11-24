structure(list(method = "POST", url = "http://localhost:6433/api/ld/reflookup", 
    status_code = 202L, headers = structure(list(date = "Tue, 24 Nov 2026 09:08:56 GMT", 
        server = "uvicorn", location = "/api/jobs/82bf05eb", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "0f0574f4"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"82bf05eb\",\"status_url\":\"/api/jobs/82bf05eb\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:1e974b6fa78dd7725ea1f8c2fe8f622db5933fff3123e6a1657a3f9874362a51\",\"query_hash\":\"sha256:1e974b6fa78dd7725ea1f8c2fe8f622db5933fff3123e6a1657a3f9874362a51\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000102, 
    starttransfer = 0.011091, total = 0.011331), cache = new.env(parent = emptyenv())), class = "httr2_response")
