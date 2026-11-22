structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 22:58:42 GMT", 
        server = "uvicorn", location = "/api/jobs/a7aa0e4a", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "0de00dff"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"a7aa0e4a\",\"status_url\":\"/api/jobs/a7aa0e4a\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:f529b92f72bf3d113fe58aedf22e675cdb8232c55217eb1f3df31fb8383fdd4d\",\"query_hash\":\"sha256:f529b92f72bf3d113fe58aedf22e675cdb8232c55217eb1f3df31fb8383fdd4d\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000187, 
    starttransfer = 0.017946, total = 0.018102), cache = new.env(parent = emptyenv())), class = "httr2_response")
