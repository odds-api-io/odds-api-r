test_that("oa_settlements flattens scores and keeps notFound", {
  log <- mock_api('{"settlements":[
    {"eventId":"74919888","sport":"football","status":"settled","final":true,"settledAt":"2026-10-01T18:00:00Z",
     "scores":{"home":2,"away":1,"periods":{"p1":{"home":1,"away":0},"ft":{"home":2,"away":1}}}},
    {"eventId":"73101436","sport":"football","status":"pending","final":false}],
    "notFound":["999"]}')
  df <- oa_settlements(c(74919888, 73101436, 999), api_key = "k")
  expect_equal(df$status, c("settled", "pending"))
  expect_equal(df$scores.periods.ft.home, c(2, NA))
  expect_equal(attr(df, "not_found"), "999")
  expect_match(last_url(log), "eventIds=74919888,73101436,999", fixed = TRUE)
  expect_error(oa_settlements(1:51, api_key = "k"), "at most 50")
})

test_that("oa_grade posts selections from a data frame and drops NA", {
  log <- mock_api('{"grades":[
    {"ref":"bet-1","eventId":"1","result":"won","reason":null,"final":true,"basis":{"scope":"regulation","home":2,"away":1}},
    {"ref":"bet-2","eventId":"1","result":"void","reason":"push","final":true}]}')
  bets <- data.frame(ref = c("bet-1", "bet-2"), eventId = c(1, 1),
                     market = c("ML", "Spread"), side = c("home", "away"),
                     hdp = c(NA, -1))
  df <- oa_grade(bets, api_key = "k")
  expect_equal(df$result, c("won", "void"))
  expect_equal(df$reason, c(NA, "push"))
  req <- log$requests[[1]]
  expect_equal(req$method, "POST")
  sent <- req$body$data
  expect_equal(length(sent$selections), 2)
  expect_null(sent$selections[[1]]$hdp)
  expect_equal(sent$selections[[2]]$hdp, -1)
  expect_error(oa_grade(data.frame(eventId = 1), api_key = "k"), "market")
})
