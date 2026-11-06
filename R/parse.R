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

oa_parse_object_df <- function(json) {
  oa_parse_df(paste0("[", json, "]"))
}

oa_json <- function(json) {
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

oa_list_df <- function(x) {
  if (!length(x)) {
    return(data.frame())
  }
  oa_parse_df(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))
}

oa_scalar <- function(v) {
  if (is.null(v)) NA else v
}

oa_bind_rows <- function(rows, template = data.frame()) {
  if (!length(rows)) {
    return(template)
  }
  cols <- unique(unlist(lapply(rows, names)))
  out <- lapply(cols, function(col) {
    vals <- lapply(rows, function(r) oa_scalar(r[[col]]))
    simple <- all(lengths(vals) == 1L) &&
      !any(vapply(vals, is.list, logical(1)))
    if (simple) unlist(vals, use.names = FALSE) else I(vals)
  })
  names(out) <- cols
  list2DF(out)
}

oa_odds_template <- function() {
  data.frame(
    eventId = integer(), bookmaker = character(), market = character(),
    updatedAt = character(), stringsAsFactors = FALSE
  )
}

oa_event_odds_rows <- function(obj) {
  rows <- list()
  for (bk in names(obj$bookmakers)) {
    for (market in obj$bookmakers[[bk]]) {
      for (line in market$odds) {
        rows[[length(rows) + 1L]] <- c(
          list(
            eventId = obj$id,
            bookmaker = bk,
            market = market$name,
            updatedAt = market$updatedAt %||% NA_character_
          ),
          lapply(line, oa_scalar)
        )
      }
    }
  }
  rows
}

oa_parse_odds <- function(json) {
  oa_bind_rows(oa_event_odds_rows(oa_json(json)), oa_odds_template())
}

oa_parse_odds_list <- function(json) {
  events <- oa_json(json)
  rows <- unlist(lapply(events, oa_event_odds_rows), recursive = FALSE)
  oa_bind_rows(rows, oa_odds_template())
}
