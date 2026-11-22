structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:15:23 GMT", 
        server = "uvicorn", location = "/api/jobs/4d571601", 
        `retry-after` = "6", `x-ogdb-cost-estimate` = "5", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "c66e5ba3"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"4d571601\",\"status_url\":\"/api/jobs/4d571601\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:d89e2e6c5ece111d06c9c70c4cd080726d926d6bc4767e4a0262acf823cf5892\",\"query_hash\":\"sha256:d89e2e6c5ece111d06c9c70c4cd080726d926d6bc4767e4a0262acf823cf5892\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 7.8e-05, 
    starttransfer = 0.012153, total = 0.012366), cache = new.env(parent = emptyenv())), class = "httr2_response")
