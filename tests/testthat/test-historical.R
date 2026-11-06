test_that("oa_historical_events requires league and caps the span at 31 days", {
  log <- mock_api(read_fixture("events.json"))
  df <- oa_historical_events("football", "england-premier-league",
                             from = as.Date("2026-09-01"),
                             to = "2026-09-30T23:59:59Z", api_key = "k")
  expect_equal(nrow(df), 2)
  expect_match(last_url(log), "/v3/historical/events?apiKey=k&sport=football&league=england-premier-league&from=2026-09-01T00:00:00Z&to=2026-09-30T23:59:59Z", fixed = TRUE)
  expect_error(oa_historical_events("football", from = "2026-09-01T00:00:00Z",
                                    to = "2026-09-02T00:00:00Z", api_key = "k"))
  expect_error(oa_historical_events("football", "x", "2026-01-01T00:00:00Z",
                                    "2026-03-01T00:00:00Z", api_key = "k"),
               "31 days")
})

test_that("oa_historical_search and oa_historical_odds", {
  log <- mock_api(read_fixture("events.json"))
  oa_historical_search("Arsenal", sport = "football", api_key = "k")
  expect_match(last_url(log), "/v3/historical/events/search?apiKey=k&query=Arsenal&sport=football", fixed = TRUE)

  log <- mock_api(read_fixture("odds.json"))
  df <- oa_historical_odds(123456, "Bet365", markets = "ML", api_key = "k")
  expect_equal(nrow(df), 4)
  expect_match(last_url(log), "/v3/historical/odds?apiKey=k&eventId=123456&bookmakers=Bet365&markets=ML", fixed = TRUE)
})

test_that("oa_closing_lines allows a season and caps at 366 days", {
  log <- mock_api(paste0("[", read_fixture("odds.json"), "]"))
  df <- oa_closing_lines("football", c("england-premier-league", "spain-laliga"),
                         from = "2025-08-01T00:00:00Z", to = "2026-05-31T00:00:00Z",
                         markets = "ML", bookmakers = c("Bet365", "SingBet"),
                         limit = 100, api_key = "k")
  expect_equal(nrow(df), 4)
  url <- last_url(log)
  expect_match(url, "leagues=england-premier-league,spain-laliga", fixed = TRUE)
  expect_match(url, "limit=100", fixed = TRUE)
  expect_error(oa_closing_lines("football", "a", "2025-01-01T00:00:00Z",
                                "2026-03-01T00:00:00Z", "ML", "Bet365",
                                api_key = "k"), "366 days")
})
