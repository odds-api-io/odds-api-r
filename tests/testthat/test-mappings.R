test_that("oa_mappings switches between single and batch ids", {
  log <- mock_api('{"data":[{"eventId":71515842,"bookmaker":"Polymarket","market":"Spread","hdp":2.5,"side":"home",
    "marketId":"0x9d","outcomeId":"104","active":true}],"nextCursor":null}')
  df <- oa_mappings("Polymarket", event_ids = 71515842, active = TRUE, api_key = "k")
  expect_equal(df$marketId, "0x9d")
  expect_null(attr(df, "next_cursor"))
  url <- last_url(log)
  expect_match(url, "eventId=71515842", fixed = TRUE)
  expect_match(url, "active=true", fixed = TRUE)

  oa_mappings("Polymarket", event_ids = c(1, 2), api_key = "k")
  expect_match(last_url(log), "eventIds=1,2", fixed = TRUE)
  expect_error(oa_mappings("Kalshi", event_ids = 1, market_ids = "x", api_key = "k"),
               "cannot be combined")
})

test_that("oa_mappings returns the bulk cursor", {
  mock_api('{"data":[],"nextCursor":"WyI3"}')
  df <- oa_mappings("Kalshi", sport = "american-football", limit = 5000, api_key = "k")
  expect_equal(nrow(df), 0)
  expect_equal(attr(df, "next_cursor"), "WyI3")
})
