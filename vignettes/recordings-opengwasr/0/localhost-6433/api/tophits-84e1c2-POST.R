structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 17:55:09 GMT", 
        server = "uvicorn", location = "/api/jobs/c800eace", 
        `retry-after` = "2", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "03f60193"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"c800eace\",\"status_url\":\"/api/jobs/c800eace\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:f529b92f72bf3d113fe58aedf22e675cdb8232c55217eb1f3df31fb8383fdd4d\",\"query_hash\":\"sha256:f529b92f72bf3d113fe58aedf22e675cdb8232c55217eb1f3df31fb8383fdd4d\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":1.08,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 7.9e-05, 
    starttransfer = 0.02247, total = 0.022512), cache = new.env(parent = emptyenv())), class = "httr2_response")
