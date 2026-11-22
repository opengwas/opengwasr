structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 19:38:51 GMT", 
        server = "uvicorn", location = "/api/jobs/397ac278", 
        `retry-after` = "4", `x-ogdb-cost-estimate` = "3", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "c4b374ed"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"397ac278\",\"status_url\":\"/api/jobs/397ac278\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:efc4384bc3db3adcafb7b5c7f6023a25e908054b00dac3044fcc00e258aeb1bc\",\"query_hash\":\"sha256:efc4384bc3db3adcafb7b5c7f6023a25e908054b00dac3044fcc00e258aeb1bc\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8.5e-05, 
    starttransfer = 0.015302, total = 0.015524), cache = new.env(parent = emptyenv())), class = "httr2_response")
