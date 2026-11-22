structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 17:55:19 GMT", 
        server = "uvicorn", location = "/api/jobs/0a4c85ea", 
        `retry-after` = "2", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "355f1a0c"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"0a4c85ea\",\"status_url\":\"/api/jobs/0a4c85ea\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:5de2a3272155ee652676aa70eb8537b3a18dbbd9740ee0f3cadc843dd9de4b2a\",\"query_hash\":\"sha256:5de2a3272155ee652676aa70eb8537b3a18dbbd9740ee0f3cadc843dd9de4b2a\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":1.05,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.5e-05, 
    starttransfer = 0.023799, total = 0.023887), cache = new.env(parent = emptyenv())), class = "httr2_response")
