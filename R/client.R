#' @keywords internal
"_PACKAGE"

oa_base_url <- "https://api.odds-api.io/v3"

oa_api_key <- function(api_key = NULL) {
  key <- api_key %||% Sys.getenv("ODDS_API_KEY")
  if (!is.character(key) || length(key) != 1 || !nzchar(key)) {
    stop(
      "No API key found. Pass `api_key` or set the ODDS_API_KEY environment ",
      "variable. Get an API key at https://odds-api.io/pricing.",
      call. = FALSE
    )
  }
  key
}

oa_optional_key <- function(api_key = NULL) {
  if (is.null(api_key) && !nzchar(Sys.getenv("ODDS_API_KEY"))) FALSE else api_key
}

`%||%` <- function(x, y) if (is.null(x)) y else x

oa_collapse <- function(x) {
  if (is.null(x)) NULL else paste(x, collapse = ",")
}

oa_flag <- function(x) {
  if (isTRUE(x)) "true" else NULL
}

oa_check_max <- function(x, max, arg) {
  if (!is.null(x) && length(x) > max) {
    stop("`", arg, "` takes at most ", max, " values, got ", length(x), ".",
         call. = FALSE)
  }
  invisible(x)
}

oa_check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a single non-empty string.", call. = FALSE)
  }
  invisible(x)
}

oa_time <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "Date")) {
    x <- as.POSIXct(format(x), tz = "UTC")
  }
  if (inherits(x, "POSIXt")) {
    return(format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  as.character(x)
}

oa_parse_time <- function(x) {
  if (is.null(x)) {
    return(NA)
  }
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(format(x), tz = "UTC"))
  }
  x <- sub("Z$", "", sub("\\.[0-9]+", "", as.character(x)))
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
}

oa_check_span <- function(from, to, max_days) {
  start <- oa_parse_time(from)
  end <- oa_parse_time(to)
  if (length(start) == 1 && length(end) == 1 && !is.na(start) && !is.na(end)) {
    span <- as.numeric(difftime(end, start, units = "days"))
    if (span > max_days) {
      stop("The date span from `from` to `to` can be at most ", max_days,
           " days.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

oa_user_agent <- function() {
  paste0(
    "oddsapiio/", utils::packageVersion("oddsapiio"),
    " R package (https://github.com/odds-api-io/odds-api-r)"
  )
}

oa_build_request <- function(path, query = list(), api_key = NULL,
                             method = "GET", body = NULL) {
  query <- Filter(Negate(is.null), query)
  req <- httr2::request(oa_base_url)
  req <- httr2::req_url_path_append(req, path)
  req <- httr2::req_user_agent(req, oa_user_agent())
  if (!identical(api_key, FALSE)) {
    query <- c(list(apiKey = oa_api_key(api_key)), query)
  }
  if (length(query)) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }
  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body, auto_unbox = TRUE, null = "null")
  }
  req <- httr2::req_method(req, method)
  req <- httr2::req_retry(
    req,
    max_tries = 3,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(502, 503, 504)
  )
  httr2::req_error(req, body = oa_error_body)
}

oa_server_message <- function(body) {
  if (is.null(body) || !nzchar(body)) {
    return(NULL)
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.list(parsed)) {
    msg <- parsed$error %||% parsed$message
    if (is.character(msg) && length(msg) == 1) {
      return(msg)
    }
  }
  body
}

oa_error_body <- function(resp) {
  status <- httr2::resp_status(resp)
  hint <- switch(
    as.character(status),
    "401" = paste0(
      "Invalid or missing API key. Check ODDS_API_KEY or the `api_key` ",
      "argument. Get an API key at https://odds-api.io/pricing."
    ),
    "403" = paste0(
      "Your API key does not have access to this endpoint. Outrights are a ",
      "paid add-on, and dropping odds, closing lines, settlement and mappings ",
      "need a paid plan. See https://odds-api.io/pricing."
    ),
    "429" = paste0(
      "Rate limit exceeded for your plan. Check ",
      "the x-ratelimit-reset response header and retry with backoff, or ",
      "upgrade at https://odds-api.io/pricing."
    ),
    NULL
  )
  body <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
  c(oa_server_message(body), hint)
}

oa_perform <- function(path, query = list(), api_key = NULL, method = "GET",
                       body = NULL) {
  httr2::req_perform(oa_build_request(path, query, api_key, method, body))
}

oa_get <- function(path, query = list(), api_key = NULL) {
  httr2::resp_body_string(oa_perform(path, query, api_key))
}

oa_header_num <- function(resp, name) {
  value <- httr2::resp_header(resp, name)
  if (is.null(value)) NA_real_ else suppressWarnings(as.numeric(value))
}
