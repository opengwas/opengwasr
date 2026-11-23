structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:03:35 GMT", 
        server = "uvicorn", location = "/api/jobs/f29671c9", 
        `retry-after` = "2", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "2de7ec9e"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"f29671c9\",\"status_url\":\"/api/jobs/f29671c9\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:7cbb4712b138ccac8ad2e45a8eb300ea8e438fd894f873768720c57e92bd3d5e\",\"query_hash\":\"sha256:7cbb4712b138ccac8ad2e45a8eb300ea8e438fd894f873768720c57e92bd3d5e\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000114, 
    starttransfer = 0.01532, total = 0.015407), cache = new.env(parent = emptyenv())), class = "httr2_response")
