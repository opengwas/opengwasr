structure(list(method = "POST", url = "http://localhost:6433/api/variants/rsid", 
    status_code = 202L, headers = structure(list(date = "Tue, 24 Nov 2026 09:08:52 GMT", 
        server = "uvicorn", location = "/api/jobs/5d4a2f88", 
        `retry-after` = "3", `x-ogdb-cost-estimate` = "4", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "e3763ca2"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"5d4a2f88\",\"status_url\":\"/api/jobs/5d4a2f88\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:b3a08496129e4e2c9cdd913fa7f887db3b474c7e7f642f42fe35e80bd907ac4a\",\"query_hash\":\"sha256:b3a08496129e4e2c9cdd913fa7f887db3b474c7e7f642f42fe35e80bd907ac4a\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000137, 
    starttransfer = 0.01343, total = 0.013652), cache = new.env(parent = emptyenv())), class = "httr2_response")
