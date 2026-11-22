structure(list(method = "POST", url = "http://localhost:6433/api/ld/matrix", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 22:58:44 GMT", 
        server = "uvicorn", location = "/api/jobs/a15c48f5", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "886ce294"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"a15c48f5\",\"status_url\":\"/api/jobs/a15c48f5\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:7577f9f9b87c18f5b5513c42c3db4abe7e9964e89035acf37ca34f299ea47b00\",\"query_hash\":\"sha256:7577f9f9b87c18f5b5513c42c3db4abe7e9964e89035acf37ca34f299ea47b00\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000202, 
    starttransfer = 0.015828, total = 0.01618), cache = new.env(parent = emptyenv())), class = "httr2_response")
