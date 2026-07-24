#' List available sports
#'
#' Returns the sports covered by 'Odds-API.io'. This endpoint does not
#' require an API key.
#'
#' @param api_key API key. Defaults to the `ODDS_API_KEY` environment
#'   variable. Not required for this endpoint.
#' @return A data frame with one row per sport (columns `name` and `slug`).
#' @examples
#' \dontrun{
#' sports <- oa_sports()
#' head(sports)
#' }
#' @export
oa_sports <- function(api_key = NULL) {
  key <- if (is.null(api_key) && !nzchar(Sys.getenv("ODDS_API_KEY"))) FALSE else api_key
  oa_parse_df(oa_get("sports", api_key = key))
}

#' List available bookmakers
#'
#' Returns the bookmakers covered by 'Odds-API.io'. This endpoint does not
#' require an API key.
#'
#' @inheritParams oa_sports
#' @return A data frame with one row per bookmaker (columns `name` and
#'   `active`).
#' @examples
#' \dontrun{
#' bookmakers <- oa_bookmakers()
#' head(bookmakers)
#' }
#' @export
oa_bookmakers <- function(api_key = NULL) {
  key <- if (is.null(api_key) && !nzchar(Sys.getenv("ODDS_API_KEY"))) FALSE else api_key
  oa_parse_df(oa_get("bookmakers", api_key = key))
}

#' List leagues for a sport
#'
#' @param sport Sport slug, e.g. `"football"`. See [oa_sports()].
#' @inheritParams oa_sports
#' @return A data frame with one row per league (columns `name`, `slug` and
#'   `eventsCount`).
#' @examples
#' \dontrun{
#' leagues <- oa_leagues("football")
#' head(leagues)
#' }
#' @export
oa_leagues <- function(sport, api_key = NULL) {
  stopifnot(is.character(sport), length(sport) == 1)
  oa_parse_df(oa_get("leagues", list(sport = sport), api_key))
}

#' List events for a sport
#'
#' Returns upcoming, live or settled events. When `to` is omitted the API
#' returns events within the next 14 days, hard-capped at 5000 rows per
#' response; use `limit` and `skip` to paginate.
#'
#' @param sport Sport slug, e.g. `"football"`. See [oa_sports()].
#' @param league Optional league slug, e.g. `"england-premier-league"`.
#' @param status Optional statuses to include: any of `"pending"`, `"live"`,
#'   `"settled"`. A vector is collapsed to a comma-separated list.
#' @param from,to Optional RFC 3339 date-times bounding the event window,
#'   e.g. `"2026-09-01T00:00:00Z"`.
#' @param limit Optional maximum number of events (capped at 5000).
#' @param skip Optional pagination offset, used together with `limit`.
#' @param bookmaker Optional bookmaker name; only events with odds from that
#'   bookmaker are returned.
#' @param participant_id Optional team or participant id to filter by.
#' @inheritParams oa_sports
#' @return A data frame with one row per event.
#' @examples
#' \dontrun{
#' events <- oa_events("football", league = "england-premier-league")
#' live <- oa_events("football", status = c("pending", "live"))
#' }
#' @export
oa_events <- function(sport, league = NULL, status = NULL, from = NULL,
                      to = NULL, limit = NULL, skip = NULL, bookmaker = NULL,
                      participant_id = NULL, api_key = NULL) {
  stopifnot(is.character(sport), length(sport) == 1)
  query <- list(
    sport = sport,
    league = league,
    status = oa_collapse(status),
    from = from,
    to = to,
    limit = limit,
    skip = skip,
    bookmaker = bookmaker,
    participantId = participant_id
  )
  oa_parse_df(oa_get("events", query, api_key))
}

#' Get odds for an event
#'
#' Fetches odds for one event from up to 30 bookmakers and returns them as a
#' tidy data frame with one row per price line.
#'
#' @param event_id Event id, from [oa_events()].
#' @param bookmakers Character vector of bookmaker names (max 30), e.g.
#'   `c("Bet365", "SingBet")`. Required by the API.
#' @inheritParams oa_sports
#' @return A data frame with one row per odds line, with columns `eventId`,
#'   `bookmaker`, `market`, `updatedAt` and the price columns present in the
#'   response (such as `home`, `draw`, `away`, `hdp`, `over`, `under`).
#' @examples
#' \dontrun{
#' odds <- oa_odds(123456, bookmakers = c("Bet365", "SingBet"))
#' subset(odds, market == "ML")
#' }
#' @export
oa_odds <- function(event_id, bookmakers, api_key = NULL) {
  stopifnot(length(event_id) == 1, length(bookmakers) >= 1)
  query <- list(eventId = event_id, bookmakers = oa_collapse(bookmakers))
  oa_parse_odds(oa_get("odds", query, api_key))
}

oa_parse_odds <- function(json) {
  obj <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  rows <- list()
  for (bk in names(obj$bookmakers)) {
    for (market in obj$bookmakers[[bk]]) {
      for (line in market$odds) {
        row <- c(
          list(
            eventId = obj$id,
            bookmaker = bk,
            market = market$name,
            updatedAt = market$updatedAt %||% NA_character_
          ),
          lapply(line, function(v) v %||% NA)
        )
        rows[[length(rows) + 1L]] <- row
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(
      eventId = integer(), bookmaker = character(), market = character(),
      updatedAt = character(), stringsAsFactors = FALSE
    ))
  }
  cols <- unique(unlist(lapply(rows, names)))
  filled <- lapply(rows, function(r) {
    r[setdiff(cols, names(r))] <- NA
    as.data.frame(r[cols], stringsAsFactors = FALSE)
  })
  do.call(rbind, filled)
}

#' Get value bets
#'
#' Returns value betting opportunities for a bookmaker, calculated by
#' comparing its odds against the market consensus.
#'
#' @param bookmaker Bookmaker name, e.g. `"Bet365"`. Required by the API.
#' @param sport Optional sport slug filter, e.g. `"baseball"`.
#' @param league Optional league slug filter; requires `sport` to be set.
#' @param include_event_details If `TRUE`, event information (teams, date,
#'   sport, league) is included in the result.
#' @inheritParams oa_sports
#' @return A data frame with one row per value bet, including the bet side,
#'   expected value and bookmaker odds.
#' @examples
#' \dontrun{
#' vb <- oa_value_bets("Bet365", include_event_details = TRUE)
#' vb[vb$expectedValue > 0.03, ]
#' }
#' @export
oa_value_bets <- function(bookmaker, sport = NULL, league = NULL,
                          include_event_details = FALSE, api_key = NULL) {
  stopifnot(is.character(bookmaker), length(bookmaker) == 1)
  if (!is.null(league) && is.null(sport)) {
    stop("`league` requires `sport` to also be set.", call. = FALSE)
  }
  query <- list(
    bookmaker = bookmaker,
    sport = sport,
    league = league,
    includeEventDetails = if (isTRUE(include_event_details)) "true" else NULL
  )
  oa_parse_df(oa_get("value-bets", query, api_key))
}

#' Get arbitrage bets
#'
#' Returns arbitrage opportunities across the given bookmakers, including
#' the legs and optimal stake split for each opportunity.
#'
#' @param bookmakers Character vector of bookmaker names to compare.
#'   Required by the API.
#' @param limit Optional maximum number of results (default 50, max 500).
#' @param include_event_details If `TRUE`, event information is included.
#' @inheritParams oa_sports
#' @return A data frame with one row per arbitrage opportunity. The `legs`
#'   and `optimalStakes` columns are list-columns of data frames.
#' @examples
#' \dontrun{
#' arbs <- oa_arbitrage_bets(c("Bet365", "SingBet"),
#'                           include_event_details = TRUE)
#' arbs[order(-arbs$profitMargin), ]
#' }
#' @export
oa_arbitrage_bets <- function(bookmakers, limit = NULL,
                              include_event_details = FALSE, api_key = NULL) {
  stopifnot(length(bookmakers) >= 1)
  query <- list(
    bookmakers = oa_collapse(bookmakers),
    limit = limit,
    includeEventDetails = if (isTRUE(include_event_details)) "true" else NULL
  )
  oa_parse_df(oa_get("arbitrage-bets", query, api_key))
}
