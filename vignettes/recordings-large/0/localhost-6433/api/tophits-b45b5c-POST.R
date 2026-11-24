structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 09:55:39 GMT", 
        server = "uvicorn", location = "/api/jobs/da27e109", 
        `retry-after` = "4", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "9c9a7112"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"da27e109\",\"status_url\":\"/api/jobs/da27e109\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:d89e2e6c5ece111d06c9c70c4cd080726d926d6bc4767e4a0262acf823cf5892\",\"query_hash\":\"sha256:d89e2e6c5ece111d06c9c70c4cd080726d926d6bc4767e4a0262acf823cf5892\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8e-05, 
    starttransfer = 0.014689, total = 0.014957), cache = new.env(parent = emptyenv())), class = "httr2_response")
