#' List historical events
#'
#' Returns finished events for one league in a date range of at most 31
#' days. Historical data is available from December 2025.
#'
#' @param sport Sport slug, e.g. `"football"`.
#' @param league League slug, e.g. `"england-premier-league"`. Required.
#' @param from,to Start and end of the window, as RFC 3339 strings such as
#'   `"2026-09-01T00:00:00Z"` or as `POSIXct`/`Date` values. At most 31
#'   days apart.
#' @inheritParams oa_sports
#' @return A data frame with one row per event, in the same format as
#'   [oa_events()], including final scores (`scores.home`, `scores.away`
#'   and period columns such as `scores.periods.p1.home` and
#'   `scores.periods.ft.home`).
#' @examples
#' \dontrun{
#' past <- oa_historical_events("football", "england-premier-league",
#'                              from = "2026-09-01T00:00:00Z",
#'                              to = "2026-09-30T23:59:59Z")
#' }
#' @export
oa_historical_events <- function(sport, league, from, to, api_key = NULL) {
  oa_check_string(sport, "sport")
  oa_check_string(league, "league")
  oa_check_span(from, to, 31)
  query <- list(sport = sport, league = league, from = oa_time(from),
                to = oa_time(to))
  oa_parse_df(oa_get("historical/events", query, api_key))
}

#' Search historical events
#'
#' Searches finished events by team name across all leagues. Returns up to
#' 100 events, newest first, from a window of at most 31 days (the API
#' defaults to the last 31 days).
#'
#' @param query Team name fragment, at least 3 characters.
#' @param sport Optional sport slug.
#' @param from,to Optional window bounds (RFC 3339 strings or
#'   `POSIXct`/`Date`), at most 31 days apart.
#' @inheritParams oa_sports
#' @return A data frame with one row per event.
#' @examples
#' \dontrun{
#' oa_historical_search("Arsenal", sport = "football")
#' }
#' @export
oa_historical_search <- function(query, sport = NULL, from = NULL, to = NULL,
                                 api_key = NULL) {
  oa_check_string(query, "query")
  if (nchar(query) < 3) {
    stop("`query` must be at least 3 characters.", call. = FALSE)
  }
  oa_check_span(from, to, 31)
  params <- list(query = query, sport = sport, from = oa_time(from),
                 to = oa_time(to))
  oa_parse_df(oa_get("historical/events/search", params, api_key))
}

#' Get historical odds for a finished event
#'
#' Returns the closing odds of a finished event.
#'
#' @param event_id Event id, from [oa_historical_events()].
#' @inheritParams oa_odds
#' @return A tidy data frame in the same format as [oa_odds()].
#' @examples
#' \dontrun{
#' past <- oa_historical_events("football", "england-premier-league",
#'                              "2026-09-01T00:00:00Z",
#'                              "2026-09-30T23:59:59Z")
#' oa_historical_odds(past$id[1], c("Bet365", "SingBet"), markets = "ML")
#' }
#' @export
oa_historical_odds <- function(event_id, bookmakers, markets = NULL,
                               api_key = NULL) {
  stopifnot(length(event_id) == 1, length(bookmakers) >= 1)
  oa_check_max(bookmakers, 30, "bookmakers")
  query <- list(
    eventId = event_id,
    bookmakers = oa_collapse(bookmakers),
    markets = oa_collapse(markets)
  )
  oa_parse_odds(oa_get("historical/odds", query, api_key))
}

#' Get closing lines for settled events
#'
#' Returns the closing odds of every settled event across up to 10 leagues
#' in one request, for closing line value and model work. The window can be
#' up to 366 days, so a full season fits in one sweep. Each response holds
#' at most 100 events; use `limit` and `skip` to paginate. Requires a paid
#' plan.
#'
#' @param sport Sport slug, e.g. `"football"`.
#' @param leagues Character vector of up to 10 league slugs.
#' @param from,to Window bounds (RFC 3339 strings or `POSIXct`/`Date`), at
#'   most 366 days apart.
#' @param markets Character vector of market names, e.g. `c("ML", "Totals")`.
#'   Required.
#' @param bookmakers Character vector of bookmaker names (max 30). Required.
#' @param limit Optional number of events per response (max 100).
#' @param skip Optional pagination offset.
#' @inheritParams oa_sports
#' @return A tidy data frame in the same format as [oa_odds()], one row per
#'   event, bookmaker, market and line.
#' @examples
#' \dontrun{
#' cl <- oa_closing_lines("football", c("england-premier-league", "spain-laliga"),
#'                        from = "2026-08-01T00:00:00Z",
#'                        to = "2026-08-31T23:59:59Z",
#'                        markets = "ML", bookmakers = c("Bet365", "SingBet"))
#' }
#' @export
oa_closing_lines <- function(sport, leagues, from, to, markets, bookmakers,
                             limit = NULL, skip = NULL, api_key = NULL) {
  oa_check_string(sport, "sport")
  stopifnot(length(leagues) >= 1, length(markets) >= 1,
            length(bookmakers) >= 1)
  oa_check_max(leagues, 10, "leagues")
  oa_check_max(bookmakers, 30, "bookmakers")
  oa_check_span(from, to, 366)
  query <- list(
    sport = sport,
    leagues = oa_collapse(leagues),
    from = oa_time(from),
    to = oa_time(to),
    markets = oa_collapse(markets),
    bookmakers = oa_collapse(bookmakers),
    limit = limit,
    skip = skip
  )
  oa_parse_odds_list(oa_get("historical/closing-lines", query, api_key))
}
