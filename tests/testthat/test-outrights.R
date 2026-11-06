test_that("oa_outrights lists tournaments", {
  log <- mock_api('[{"id":1001,"name":"Mexico Open","date":"2026-11-20T12:00:00Z","status":"pending",
    "sport":{"name":"Golf","slug":"golf"},"league":{"name":"PGA Tour","slug":"pga-tour"},"bookmakerCount":5}]')
  df <- oa_outrights("golf", league = "pga-tour", status = c("pending", "live"), api_key = "k")
  expect_equal(df$bookmakerCount, 5)
  expect_match(last_url(log), "/v3/outrights?apiKey=k&sport=golf&league=pga-tour&status=pending,live", fixed = TRUE)
})

test_that("oa_outright_odds gives one row per bookmaker, market and golfer", {
  mock_api('{"id":1001,"name":"Mexico Open","bookmakers":{
    "Betfair Exchange":[
      {"name":"Winner","updatedAt":"2026-11-10T10:00:00Z","odds":[
        {"label":"Ben Griffin","participantId":991291386379,"odds":"18.50","layOdds":"19.00"},
        {"label":"Other Golfer","participantId":2,"odds":"25.0","layOdds":"27.0"}]},
      {"name":"Make Cut","updatedAt":"2026-11-10T10:00:00Z","odds":[
        {"label":"Ben Griffin","participantId":991291386379,"yes":"1.149","no":"6.667"}]}]}}')
  df <- oa_outright_odds(1001, "Betfair Exchange", api_key = "k")
  expect_equal(nrow(df), 3)
  expect_equal(df$market, c("Winner", "Winner", "Make Cut"))
  expect_equal(df$participantId[1], 991291386379)
  expect_equal(df$yes, c(NA, NA, "1.149"))
})

test_that("oa_outright_results returns results and winners", {
  mock_api('{"id":1001,"name":"Mexico Open","status":"settled",
    "results":[{"label":"A","participantId":1,"position":1,"tied":false,"madeCut":true,"status":"finished"},
               {"label":"B","participantId":2,"position":null,"tied":false,"madeCut":false,"status":"cut"}],
    "winners":{"Winner":[1],"Top 5":[1],"Make Cut":[1]}}')
  df <- oa_outright_results(1001, api_key = "k")
  expect_equal(df$position, c(1, NA))
  expect_equal(attr(df, "winners")[["Top 5"]], 1)
  expect_equal(attr(df, "status"), "settled")
})

test_that("a 403 for the outrights add-on passes the server text through", {
  mock_api('{"error":"Outrights are a paid add-on. Add it in your dashboard: https://odds-api.io/dashboard/billing#outrights"}',
           status = 403L)
  err <- expect_error(oa_outrights("golf", api_key = "k"), class = "httr2_http_403")
  msg <- conditionMessage(err)
  expect_match(msg, "Outrights are a paid add-on", fixed = TRUE)
  expect_match(msg, "dashboard/billing#outrights", fixed = TRUE)
})
