structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 09:54:59 GMT", 
        server = "uvicorn", location = "/api/jobs/fe1d4de0", 
        `retry-after` = "3", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "f5a9cc27"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"fe1d4de0\",\"status_url\":\"/api/jobs/fe1d4de0\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:2a5318fda884c2fdee348118e3d329c1a4480dc43e8d985a306ce7403ef94758\",\"query_hash\":\"sha256:2a5318fda884c2fdee348118e3d329c1a4480dc43e8d985a306ce7403ef94758\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.6e-05, 
    starttransfer = 0.0167, total = 0.017009), cache = new.env(parent = emptyenv())), class = "httr2_response")
