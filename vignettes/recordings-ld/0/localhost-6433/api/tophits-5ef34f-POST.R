structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:03:47 GMT", 
        server = "uvicorn", location = "/api/jobs/d7fb0785", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "1621b6ea"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"d7fb0785\",\"status_url\":\"/api/jobs/d7fb0785\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:739d1d3c922fff2690b9d3e3f216303a3e109aa862ac786cb13f033fc6a398b4\",\"query_hash\":\"sha256:739d1d3c922fff2690b9d3e3f216303a3e109aa862ac786cb13f033fc6a398b4\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.3e-05, 
    starttransfer = 0.013779, total = 0.014044), cache = new.env(parent = emptyenv())), class = "httr2_response")
