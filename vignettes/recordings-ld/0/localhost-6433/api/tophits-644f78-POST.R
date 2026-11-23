structure(list(method = "POST", url = "http://localhost:6433/api/tophits", 
    status_code = 422L, headers = structure(list(date = "Mon, 23 Nov 2026 09:55:02 GMT", 
        server = "uvicorn", `content-length` = "692", `content-type` = "application/json", 
        `x-request-id` = "1b93b83c"), class = "httr2_headers"), 
    body = charToRaw("{\"error\":\"ld_refusal\",\"reason\":\"threshold_below_t_far\",\"message\":\"r2=0.001 is below t_far=0.03186544342507645: beyond 3,000,000 bp the store holds only the Bound r² < t_far for unstored pairs, which does not decide r̂² ≥ r2\",\"store\":\"hgdp1kgp-v2-EUR-k10\",\"ancestry\":\"EUR\",\"t_far\":0.03186544342507645,\"near_bp\":3000000,\"far_bp\":10000000,\"detail\":{\"error\":\"ld_refusal\",\"reason\":\"threshold_below_t_far\",\"message\":\"r2=0.001 is below t_far=0.03186544342507645: beyond 3,000,000 bp the store holds only the Bound r² < t_far for unstored pairs, which does not decide r̂² ≥ r2\",\"store\":\"hgdp1kgp-v2-EUR-k10\",\"ancestry\":\"EUR\",\"t_far\":0.03186544342507645,\"near_bp\":3000000,\"far_bp\":10000000}}"), 
    timing = c(redirect = 0, namelookup = 0, connect = 0, pretransfer = 8e-05, 
    starttransfer = 0.005493, total = 0.005614), cache = new.env(parent = emptyenv())), class = "httr2_response")
