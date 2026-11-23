structure(list(method = "POST", url = "http://localhost:6433/api/ld/clump", 
    status_code = 202L, headers = structure(list(date = "Mon, 23 Nov 2026 09:55:29 GMT", 
        server = "uvicorn", location = "/api/jobs/840da636", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "bb938ca5"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"840da636\",\"status_url\":\"/api/jobs/840da636\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:0f292f15fda03e8ac2971a90451b0a758cf76ed67e74800fee93fb67f43ce6f0\",\"query_hash\":\"sha256:0f292f15fda03e8ac2971a90451b0a758cf76ed67e74800fee93fb67f43ce6f0\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000105, 
    starttransfer = 0.015397, total = 0.015513), cache = new.env(parent = emptyenv())), class = "httr2_response")
