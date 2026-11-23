structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:03:51 GMT", 
        server = "uvicorn", location = "/api/jobs/ebd4786f", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "3699b79d"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"ebd4786f\",\"status_url\":\"/api/jobs/ebd4786f\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:28e61cd092c165120d50486fa71a7ee83e6cd8388e48a9d9afdf9f9d07fa4f9b\",\"query_hash\":\"sha256:28e61cd092c165120d50486fa71a7ee83e6cd8388e48a9d9afdf9f9d07fa4f9b\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.4e-05, 
    starttransfer = 0.012812, total = 0.012921), cache = new.env(parent = emptyenv())), class = "httr2_response")
