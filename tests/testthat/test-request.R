test_that("oa_build_request builds the expected URL", {
  req <- oddsapiio:::oa_build_request(
    "events",
    list(sport = "football", league = "england-premier-league"),
    api_key = "test-key"
  )
  expect_match(req$url, "^https://api\\.odds-api\\.io/v3/events\\?")
  expect_match(req$url, "apiKey=test-key", fixed = TRUE)
  expect_match(req$url, "sport=football", fixed = TRUE)
  expect_match(req$url, "league=england-premier-league", fixed = TRUE)
})

test_that("NULL query params are dropped", {
  req <- oddsapiio:::oa_build_request(
    "events",
    list(sport = "football", league = NULL, limit = NULL),
    api_key = "k"
  )
  expect_false(grepl("league", req$url))
  expect_false(grepl("limit", req$url))
})

test_that("vector params collapse to comma-separated values", {
  expect_identical(oddsapiio:::oa_collapse(c("Bet365", "SingBet")), "Bet365,SingBet")
  expect_null(oddsapiio:::oa_collapse(NULL))
})

test_that("missing API key gives a helpful error", {
  withr_old <- Sys.getenv("ODDS_API_KEY")
  Sys.setenv(ODDS_API_KEY = "")
  on.exit(Sys.setenv(ODDS_API_KEY = withr_old))
  expect_error(oddsapiio:::oa_api_key(NULL), "ODDS_API_KEY")
})

test_that("league without sport errors locally in oa_value_bets", {
  expect_error(
    oa_value_bets("Bet365", league = "usa-mlb", api_key = "k"),
    "requires `sport`"
  )
})
