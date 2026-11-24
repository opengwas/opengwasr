structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 09:55:15 GMT", 
        server = "uvicorn", location = "/api/jobs/ca02b9f8", 
        `retry-after` = "3", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "ea5ff191"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"ca02b9f8\",\"status_url\":\"/api/jobs/ca02b9f8\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:7c5b4105fec91e1461db659dacbab643624a2520657264db60208eadaeb8eee2\",\"query_hash\":\"sha256:7c5b4105fec91e1461db659dacbab643624a2520657264db60208eadaeb8eee2\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.1e-05, 
    starttransfer = 0.016361, total = 0.01645), cache = new.env(parent = emptyenv())), class = "httr2_response")
