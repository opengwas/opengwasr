structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 09:55:31 GMT", 
        server = "uvicorn", location = "/api/jobs/0de352d1", 
        `retry-after` = "4", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "1243d661"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"0de352d1\",\"status_url\":\"/api/jobs/0de352d1\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:e580cca27e6890f37389b968b946b1bcaf6f9ce5561979cecb21d5768033440f\",\"query_hash\":\"sha256:e580cca27e6890f37389b968b946b1bcaf6f9ce5561979cecb21d5768033440f\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.3e-05, 
    starttransfer = 0.013512, total = 0.013652), cache = new.env(parent = emptyenv())), class = "httr2_response")
