read_fixture <- function(name) {
  paste(readLines(test_path("fixtures", name), warn = FALSE), collapse = "\n")
}

json_response <- function(body, status = 200L, headers = list()) {
  httr2::response(
    status_code = status,
    headers = c(list(`Content-Type` = "application/json"), headers),
    body = charToRaw(enc2utf8(body))
  )
}

# Mocks the HTTP layer and records every request in `env$requests`.
mock_api <- function(body, status = 200L, headers = list(),
                     env = parent.frame()) {
  log <- new.env()
  log$requests <- list()
  httr2::local_mocked_responses(function(req) {
    log$requests[[length(log$requests) + 1L]] <- req
    json_response(body, status, headers)
  }, env = env)
  log
}

last_url <- function(log) {
  utils::URLdecode(log$requests[[length(log$requests)]]$url)
}
