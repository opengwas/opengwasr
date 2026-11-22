structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:03:23 GMT", 
        server = "uvicorn", location = "/api/jobs/56dc2108", 
        `retry-after` = "6", `x-ogdb-cost-estimate` = "7", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "330744ea"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"56dc2108\",\"status_url\":\"/api/jobs/56dc2108\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:2cc05d402fd2aedf1740d63a320d0a7e213d52ebfd2c831257c505591f0920ff\",\"query_hash\":\"sha256:2cc05d402fd2aedf1740d63a320d0a7e213d52ebfd2c831257c505591f0920ff\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 9.2e-05, 
    starttransfer = 0.017666, total = 0.017848), cache = new.env(parent = emptyenv())), class = "httr2_response")
