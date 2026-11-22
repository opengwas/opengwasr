structure(list(method = "POST", url = "http://localhost:6433/api/associations", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:39:04 GMT", 
        server = "uvicorn", location = "/api/jobs/03faf936", 
        `retry-after` = "4", `x-ogdb-cost-estimate` = "4", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "3f01bae7"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"03faf936\",\"status_url\":\"/api/jobs/03faf936\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:15da0a688f519f238748db4aa1e7d072ff80616d3a9eef8eab40379794dbb7ed\",\"query_hash\":\"sha256:15da0a688f519f238748db4aa1e7d072ff80616d3a9eef8eab40379794dbb7ed\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000101, 
    starttransfer = 0.027408, total = 0.027647), cache = new.env(parent = emptyenv())), class = "httr2_response")
