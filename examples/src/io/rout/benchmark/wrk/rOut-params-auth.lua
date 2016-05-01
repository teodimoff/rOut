-- example HTTP POST script which demonstrates setting the
-- HTTP method, body, and adding a header

wrk.method = "POST"
wrk.body   = "s=foo&i=10&b=false"
wrk.headers["Content-Type"] = "application/x-www-form-urlencoded"
wrk.headers["Cookie"] = "password=1d2a409b"