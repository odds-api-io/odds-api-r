read_fixture <- function(name) {
  paste(readLines(test_path("fixtures", name), warn = FALSE), collapse = "\n")
}

test_that("event arrays parse to a tidy data frame", {
  df <- oddsapiio:::oa_parse_df(read_fixture("events.json"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
  expect_true(all(c("id", "home", "away", "status") %in% names(df)))
  expect_true("sport.slug" %in% names(df))
  expect_equal(df$id, c(123456, 123457))
})

test_that("empty arrays parse to an empty data frame", {
  df <- oddsapiio:::oa_parse_df("[]")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
})

test_that("odds responses flatten to one row per line", {
  df <- oddsapiio:::oa_parse_odds(read_fixture("odds.json"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4)
  expect_setequal(unique(df$bookmaker), c("Bet365", "SingBet"))
  ml <- df[df$market == "ML", ]
  expect_equal(ml$home, "2.10")
  expect_equal(ml$draw, "3.40")
  totals <- df[df$market == "Totals", ]
  expect_equal(totals$hdp, c(2.5, 3))
  expect_true(all(is.na(ml$hdp)))
  expect_true(all(df$eventId == 123456))
})

test_that("odds response with no bookmakers gives empty frame", {
  df <- oddsapiio:::oa_parse_odds('{"id": 1, "bookmakers": {}}')
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
})
