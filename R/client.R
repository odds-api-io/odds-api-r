#' @keywords internal
"_PACKAGE"

oa_base_url <- "https://api.odds-api.io/v3"

oa_api_key <- function(api_key = NULL) {
  key <- api_key %||% Sys.getenv("ODDS_API_KEY")
  if (!is.character(key) || length(key) != 1 || !nzchar(key)) {
    stop(
      "No API key found. Pass `api_key` or set the ODDS_API_KEY environment ",
      "variable. Get a free key at https://odds-api.io.",
      call. = FALSE
    )
  }
  key
}

`%||%` <- function(x, y) if (is.null(x)) y else x

oa_collapse <- function(x) {
  if (is.null(x)) NULL else paste(x, collapse = ",")
}

oa_build_request <- function(path, query = list(), api_key = NULL) {
  query <- Filter(Negate(is.null), query)
  req <- httr2::request(oa_base_url)
  req <- httr2::req_url_path_append(req, path)
  req <- httr2::req_user_agent(
    req,
    paste0("oddsapiio R package (https://github.com/odds-api-io/odds-api-r)")
  )
  if (!identical(api_key, FALSE)) {
    query <- c(list(apiKey = oa_api_key(api_key)), query)
  }
  if (length(query)) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }
  httr2::req_error(req, body = oa_error_body)
}

oa_error_body <- function(resp) {
  status <- httr2::resp_status(resp)
  hint <- switch(
    as.character(status),
    "401" = paste0(
      "Invalid or missing API key. Check ODDS_API_KEY or the `api_key` ",
      "argument. Get a free key at https://odds-api.io."
    ),
    "403" = paste0(
      "Your API key does not have access to this endpoint. Some endpoints, ",
      "such as dropping odds, require a paid plan. The free tier covers the ",
      "core endpoints at 100 requests/hour. See https://odds-api.io/pricing."
    ),
    "429" = paste0(
      "Rate limit exceeded. The free tier allows 100 requests/hour. Check ",
      "the x-ratelimit-reset response header and retry with backoff, or ",
      "upgrade at https://odds-api.io/pricing."
    ),
    NULL
  )
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
  paste(c(hint, body), collapse = "\n")
}

oa_get <- function(path, query = list(), api_key = NULL) {
  resp <- httr2::req_perform(oa_build_request(path, query, api_key))
  httr2::resp_body_string(resp)
}

oa_parse_df <- function(json) {
  out <- jsonlite::fromJSON(json, flatten = TRUE)
  if (is.data.frame(out)) {
    return(out)
  }
  if (is.list(out) && length(out) == 0) {
    return(data.frame())
  }
  out
}
