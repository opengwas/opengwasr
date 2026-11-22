structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:03:33 GMT", 
        server = "uvicorn", location = "/api/jobs/b296d080", 
        `retry-after` = "4", `x-ogdb-cost-estimate` = "4", `content-length` = "396", 
        `content-type` = "application/json", `x-request-id` = "2ec9b4da"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"b296d080\",\"status_url\":\"/api/jobs/b296d080\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:5de2a3272155ee652676aa70eb8537b3a18dbbd9740ee0f3cadc843dd9de4b2a\",\"query_hash\":\"sha256:5de2a3272155ee652676aa70eb8537b3a18dbbd9740ee0f3cadc843dd9de4b2a\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 9.9e-05, 
    starttransfer = 0.013911, total = 0.014005), cache = new.env(parent = emptyenv())), class = "httr2_response")
