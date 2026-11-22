structure(list(method = "POST", url = "http://localhost:6433/api/associations", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:39:04 GMT", 
        server = "uvicorn", location = "/api/jobs/1b061fc0", 
        `retry-after` = "2", `x-ogdb-cost-estimate` = "2", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "8b1c2d6d"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"1b061fc0\",\"status_url\":\"/api/jobs/1b061fc0\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:a4d8a4f3c4e31240a3fb578dac918633aabe55a6c47899006ad6c389812e8517\",\"query_hash\":\"sha256:a4d8a4f3c4e31240a3fb578dac918633aabe55a6c47899006ad6c389812e8517\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 9.6e-05, 
    starttransfer = 0.017041, total = 0.01731), cache = new.env(parent = emptyenv())), class = "httr2_response")
