structure(list(method = "POST", url = "http://localhost:6433/api/variants/rsid", 
    status_code = 202L, headers = structure(list(date = "Sun, 22 Nov 2026 22:58:33 GMT", 
        server = "uvicorn", location = "/api/jobs/ad3f3eb3", 
        `retry-after` = "1", `x-ogdb-cost-estimate` = "1", `content-length` = "397", 
        `content-type` = "application/json", `x-request-id` = "fd26f770"), class = "httr2_headers"), 
    body = charToRaw("{\"job_id\":\"ad3f3eb3\",\"status_url\":\"/api/jobs/ad3f3eb3\",\"status\":\"queued\",\"request_fingerprint\":\"sha256:9059ce2715d29bde975eac0425b840cf5fad208844d061b12bfcc23946ce0057\",\"query_hash\":\"sha256:9059ce2715d29bde975eac0425b840cf5fad208844d061b12bfcc23946ce0057\",\"existing\":false,\"queue_position\":null,\"poll_after_seconds\":0,\"ignored_fields\":[]}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 0.000105, 
    starttransfer = 0.018423, total = 0.018518), cache = new.env(parent = emptyenv())), class = "httr2_response")
