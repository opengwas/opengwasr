structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 00:03:41 GMT", 
        server = "uvicorn", location = "/api/jobs/0120469e", 
        `retry-after` = "2", `x-ogdb-cost-estimate` = "1", `content-length` = "396", 
        `content-type` = "application/json", `x-request-id` = "322ac5ff"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"0120469e\",\"status_url\":\"/api/jobs/0120469e\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:453b17294996e198b7eccac5fec9fd8580e721861aeef2425696f4f337918cc7\",\"query_hash\":\"sha256:453b17294996e198b7eccac5fec9fd8580e721861aeef2425696f4f337918cc7\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 7.9e-05, 
    starttransfer = 0.01546, total = 0.015577), cache = new.env(parent = emptyenv())), class = "httr2_response")
