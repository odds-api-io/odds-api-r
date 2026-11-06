test_that("oa_odds sends markets and returns tidy rows", {
  log <- mock_api(read_fixture("odds.json"))
  df <- oa_odds(123456, c("Bet365", "SingBet"), markets = c("ML", "Totals"),
                api_key = "k")
  expect_equal(nrow(df), 4)
  expect_match(last_url(log), "markets=ML,Totals", fixed = TRUE)
  expect_error(oa_odds(1, "Bet365", markets = paste0("m", 1:41), api_key = "k"),
               "at most 40")
})

test_that("oa_multi_odds stacks every event", {
  ev <- read_fixture("odds.json")
  ev2 <- sub('"id": 123456', '"id": 123457', ev)
  log <- mock_api(paste0("[", ev, ",", ev2, "]"))
  df <- oa_multi_odds(c(123456, 123457), "Bet365", api_key = "k")
  expect_equal(nrow(df), 8)
  expect_setequal(unique(df$eventId), c(123456, 123457))
  expect_match(last_url(log), "/v3/odds/multi?apiKey=k&eventIds=123456,123457", fixed = TRUE)
  expect_error(oa_multi_odds(1:11, "Bet365", api_key = "k"), "at most 10")
})

test_that("oa_odds_updated requires markets and exposes X-Next-Since", {
  expect_error(oa_odds_updated(1, "Bet365", "football", api_key = "k"),
               "`markets` is required")
  log <- mock_api(paste0("[", read_fixture("odds.json"), "]"),
                  headers = list(`X-Next-Since` = "1790000000",
                                 `X-Server-Time` = "1790000010"))
  df <- oa_odds_updated(1789999990, "Bet365", "football",
                        markets = c("ML", "Totals"), api_key = "k")
  expect_equal(attr(df, "next_since"), 1790000000)
  expect_equal(attr(df, "server_time"), 1790000010)
  url <- last_url(log)
  expect_match(url, "since=1789999990", fixed = TRUE)
  expect_match(url, "markets=ML,Totals", fixed = TRUE)
})

test_that("oa_odds_movements returns history with opening and latest", {
  log <- mock_api('{"eventid":"1","bookmaker":"SingBet",
    "opening":{"home":2.0,"draw":3.2,"away":3.5,"timestamp":1734400000000},
    "latest":{"home":2.1,"draw":3.3,"away":3.4,"timestamp":1734450000000},
    "movements":[{"home":2.0,"draw":3.2,"away":3.5,"timestamp":1734400000000},
                 {"home":2.1,"draw":3.3,"away":3.4,"timestamp":1734450000000}]}')
  df <- oa_odds_movements(1, "SingBet", "ML", api_key = "k")
  expect_equal(df$home, c(2.0, 2.1))
  expect_s3_class(df$time, "POSIXct")
  expect_equal(attr(df, "opening")$home, 2.0)
  expect_match(last_url(log), "market=ML", fixed = TRUE)
})

test_that("oa_odds_summary flattens markets, lines and outcomes", {
  body <- '{"eventId":"1","home":"A","away":"B","demo":true,"markets":[
    {"name":"Totals","lines":[{"hdp":2.5,"main":true,"books":12,"fairBooks":10,
      "payout":{"best":1.02,"average":0.95},
      "outcomes":{"over":{"best":1.95,"bestBookmakers":["Bet365","Unibet"],"average":1.88,"median":1.9,"worst":1.8,"fair":1.92},
                  "under":{"best":2.0,"bestBookmakers":["SingBet"],"average":1.9,"median":1.9,"worst":1.85,"fair":1.97}},
      "exchanges":{"over":{"best":1.97,"venue":"Betfair Exchange","liquidity":500,"currency":"GBP"}}}]}]}'
  log <- mock_api(body)
  df <- oa_odds_summary(1, markets = "Totals", include_exchanges = TRUE,
                        api_key = "k")
  expect_equal(df$outcome, c("over", "under"))
  expect_equal(df$bestBookmakers[1], "Bet365,Unibet")
  expect_equal(df$exchangeVenue, c("Betfair Exchange", NA))
  expect_equal(df$payoutBest, c(1.02, 1.02))
  expect_true(attr(df, "demo"))
  expect_match(last_url(log), "includeExchanges=true", fixed = TRUE)

  log <- mock_api(paste0("[", body, "]"))
  df <- oa_odds_summary_multi(c(1, 2), lines = "all", api_key = "k")
  expect_equal(nrow(df), 2)
  expect_match(last_url(log), "/v3/odds/summary/multi?apiKey=k&eventIds=1,2&lines=all", fixed = TRUE)
})

test_that("oa_arbitrage_bets sends the new filters", {
  log <- mock_api("[]")
  oa_arbitrage_bets(c("Bet365", "SingBet"), sport = "football",
                    markets = "Totals", min_profit = 1.5,
                    all_combinations = TRUE, api_key = "k")
  url <- last_url(log)
  for (p in c("sport=football", "markets=Totals", "minProfit=1.5",
              "allCombinations=true")) {
    expect_match(url, p, fixed = TRUE)
  }
  expect_error(oa_arbitrage_bets("Bet365", league = "x", api_key = "k"),
               "requires `sport`")
})

test_that("oa_dropping_odds sends markets and reads pagination headers", {
  log <- mock_api('[{"eventId":1,"market":{"name":"Spread","hdp":-1.5},"betSide":"home",
    "odds":{"opening":2.05,"current":1.52,"drop":{"sinceOpening":25.9}}}]',
    headers = list(`X-Total-Count` = "120", `X-Page` = "2", `X-Per-Page` = "50"))
  df <- oa_dropping_odds("football", leagues = c("a", "b"),
                         markets = c("Spread", "Totals"), time_window = "24h",
                         sort = "recent", page = 2, api_key = "k")
  expect_equal(df$market.name, "Spread")
  expect_equal(attr(df, "total_count"), 120)
  url <- last_url(log)
  expect_match(url, "markets=Spread,Totals", fixed = TRUE)
  expect_match(url, "leagues=a,b", fixed = TRUE)
  expect_match(url, "timeWindow=24h", fixed = TRUE)
  expect_error(oa_dropping_odds("football", league = "a", leagues = "b",
                                api_key = "k"), "either")
})
