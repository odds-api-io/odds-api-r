test_that("oa_markets parses one sport and the all-sports map", {
  log <- mock_api('{"sport":{"name":"Football","slug":"football"},"markets":[
    {"name":"ML","shape":"moneyline","period":"match","prematch":true,"live":true},
    {"name":"Totals HT","shape":"line","period":"first-half","prematch":true,"live":false}]}')
  df <- oa_markets("football", api_key = "k")
  expect_equal(df$name, c("ML", "Totals HT"))
  expect_equal(df$sport, c("football", "football"))
  expect_equal(df$live, c(TRUE, FALSE))
  expect_match(last_url(log), "/v3/markets?apiKey=k&sport=football", fixed = TRUE)

  mock_api('{"basketball":[{"name":"ML","shape":"moneyline","period":"match","prematch":true,"live":true}],
             "football":[{"name":"ML","shape":"moneyline","period":"match","prematch":true,"live":true}]}')
  all <- oa_markets(api_key = "k")
  expect_equal(all$sport, c("basketball", "football"))
})

test_that("oa_sports works without an API key", {
  withr_old <- Sys.getenv("ODDS_API_KEY")
  Sys.setenv(ODDS_API_KEY = "")
  on.exit(Sys.setenv(ODDS_API_KEY = withr_old))
  log <- mock_api('[{"name":"Football","slug":"football"}]')
  df <- oa_sports()
  expect_equal(df$slug, "football")
  expect_false(grepl("apiKey", last_url(log)))
})

test_that("oa_leagues sends all=true", {
  log <- mock_api('[{"name":"England - Premier League","slug":"england-premier-league","eventsCount":10}]')
  df <- oa_leagues("football", all = TRUE, api_key = "k")
  expect_equal(df$eventsCount, 10)
  expect_match(last_url(log), "all=true", fixed = TRUE)
})

test_that("oa_events formats POSIXct dates and status lists", {
  log <- mock_api(read_fixture("events.json"))
  df <- oa_events("football", status = c("settled", "cancelled"),
                  from = as.POSIXct("2026-09-01 12:00:00", tz = "UTC"),
                  api_key = "k")
  expect_equal(nrow(df), 2)
  url <- last_url(log)
  expect_match(url, "status=settled,cancelled", fixed = TRUE)
  expect_match(url, "from=2026-09-01T12:00:00Z", fixed = TRUE)
})

test_that("event lookup functions hit the right paths", {
  log <- mock_api('{"id":123456,"home":"A","away":"B","status":"live","scores":{"home":1,"away":0,"periods":{"p1":{"home":1,"away":0}}}}')
  ev <- oa_event(123456, api_key = "k")
  expect_equal(nrow(ev), 1)
  expect_equal(ev$scores.periods.p1.home, 1)
  expect_match(last_url(log), "/v3/events/123456?", fixed = TRUE)

  log <- mock_api(read_fixture("events.json"))
  oa_live_events("football", api_key = "k")
  expect_match(last_url(log), "/v3/events/live?apiKey=k&sport=football", fixed = TRUE)
  oa_search_events("Liverpool", status = "live", api_key = "k")
  expect_match(last_url(log), "/v3/events/search?apiKey=k&query=Liverpool&status=live", fixed = TRUE)
  expect_error(oa_search_events("Li", api_key = "k"), "at least 3")
})

test_that("participants and selected bookmakers", {
  log <- mock_api('[{"id":45,"name":"Chelsea","sport":"football"}]')
  df <- oa_participants("football", search = "Chelsea", api_key = "k")
  expect_equal(df$id, 45)
  expect_match(last_url(log), "search=Chelsea", fixed = TRUE)

  log <- mock_api('{"id":45,"name":"Chelsea","sport":"football"}')
  expect_equal(oa_participant(45, api_key = "k")$name, "Chelsea")
  expect_match(last_url(log), "/v3/participants/45?", fixed = TRUE)

  log <- mock_api('{"bookmakers":["Bet365","SingBet"],"count":2}')
  expect_equal(oa_selected_bookmakers(api_key = "k"), c("Bet365", "SingBet"))
  expect_equal(oa_select_bookmakers(c("Bet365", "SingBet"), api_key = "k"),
               c("Bet365", "SingBet"))
  req <- log$requests[[2]]
  expect_equal(req$method, "PUT")
  expect_match(utils::URLdecode(req$url), "selected/select?apiKey=k&bookmakers=Bet365,SingBet", fixed = TRUE)
})
