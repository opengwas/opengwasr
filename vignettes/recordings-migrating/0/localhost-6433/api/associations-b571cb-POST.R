structure(list(method = "POST", url = "http://localhost:6433/api/associations", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:05:07 GMT", 
        server = "uvicorn", location = "/api/jobs/1c4135cf", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "2bce4d44"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"1c4135cf\",\"status_url\":\"/api/jobs/1c4135cf\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:15470ee314bc3344c2c758ebb684a34551c9e769f8c7aecf9d31cb0bf9695556\",\"query_hash\":\"sha256:15470ee314bc3344c2c758ebb684a34551c9e769f8c7aecf9d31cb0bf9695556\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 9.9e-05, 
    starttransfer = 0.016659, total = 0.016808), cache = new.env(parent = emptyenv())), class = "httr2_response")
