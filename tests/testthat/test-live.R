skip_live <- function() {
  skip_on_cran()
  skip_if(Sys.getenv("ODDS_API_KEY") == "", "ODDS_API_KEY not set")
}

test_that("live: oa_sports returns sports", {
  skip_live()
  df <- oa_sports()
  expect_s3_class(df, "data.frame")
  expect_true("football" %in% df$slug)
})

test_that("live: oa_bookmakers returns bookmakers", {
  skip_live()
  df <- oa_bookmakers()
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 100)
})

test_that("live: oa_leagues and oa_events work for football", {
  skip_live()
  leagues <- oa_leagues("football")
  expect_s3_class(leagues, "data.frame")
  expect_true("slug" %in% names(leagues))
  events <- oa_events("football", limit = 5)
  expect_s3_class(events, "data.frame")
  expect_true(nrow(events) <= 5)
})

test_that("live: oa_odds returns tidy odds", {
  skip_live()
  events <- oa_events("football", bookmaker = "Bet365", limit = 1)
  skip_if(nrow(events) == 0, "no events available")
  odds <- oa_odds(events$id[1], bookmakers = "Bet365")
  expect_s3_class(odds, "data.frame")
  expect_true(all(c("bookmaker", "market") %in% names(odds)))
})
