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
  oa_parse_df(oa_get("sports", api_key = oa_optional_key(api_key)))
}

#' List available bookmakers
#'
#' Returns the bookmakers covered by 'Odds-API.io'. This endpoint does not
#' require an API key.
#'
#' @inheritParams oa_sports
#' @return A data frame with one row per bookmaker, with columns `name`,
#'   `active`, `createdAt`, `status` (`"ok"`, `"degraded"` or `"down"`) and
#'   `reason` (`NA` when the status is ok, otherwise `"maintenance"` or
#'   `"unavailable"`).
#' @examples
#' \dontrun{
#' bookmakers <- oa_bookmakers()
#' subset(bookmakers, status != "ok")
#' }
#' @export
oa_bookmakers <- function(api_key = NULL) {
  oa_parse_df(oa_get("bookmakers", api_key = oa_optional_key(api_key)))
}

#' Manage the bookmakers selected on your API key
#'
#' Plans include a fixed number of bookmakers. `oa_selected_bookmakers()`
#' lists the bookmakers selected on your key, `oa_select_bookmakers()` adds
#' bookmakers to the selection and `oa_clear_selected_bookmakers()` clears
#' it. Changing the selection is limited by your plan.
#'
#' @param bookmakers Character vector of bookmaker names, e.g.
#'   `c("Bet365", "SingBet")`.
#' @inheritParams oa_sports
#' @return A character vector of the selected bookmaker names (invisibly for
#'   `oa_select_bookmakers()` and `oa_clear_selected_bookmakers()`).
#' @examples
#' \dontrun{
#' oa_selected_bookmakers()
#' oa_select_bookmakers(c("Bet365", "SingBet"))
#' }
#' @export
oa_selected_bookmakers <- function(api_key = NULL) {
  oa_selection(oa_get("bookmakers/selected", api_key = api_key))
}

#' @rdname oa_selected_bookmakers
#' @export
oa_select_bookmakers <- function(bookmakers, api_key = NULL) {
  stopifnot(is.character(bookmakers), length(bookmakers) >= 1)
  resp <- oa_perform(
    "bookmakers/selected/select",
    list(bookmakers = oa_collapse(bookmakers)),
    api_key,
    method = "PUT"
  )
  invisible(oa_selection(httr2::resp_body_string(resp)))
}

#' @rdname oa_selected_bookmakers
#' @export
oa_clear_selected_bookmakers <- function(api_key = NULL) {
  resp <- oa_perform("bookmakers/selected/clear", api_key = api_key,
                     method = "PUT")
  invisible(oa_selection(httr2::resp_body_string(resp)))
}

oa_selection <- function(json) {
  as.character(unlist(oa_json(json)$bookmakers))
}

#' List markets
#'
#' Returns the market catalogue: every market currently served, with how to
#' parse it. Use it to look up exact market names for the `markets` argument
#' of [oa_odds()] and related functions. Market names are, for example,
#' `"ML"`, `"Spread"`, `"Totals"`, `"Both Teams To Score"` and `"Totals HT"`.
#' This endpoint does not require an API key.
#'
#' @param sport Optional sport slug, e.g. `"football"`. When omitted, markets
#'   for all sports are returned.
#' @inheritParams oa_sports
#' @return A data frame with one row per market and columns `sport`, `name`,
#'   `shape` (`"moneyline"`, `"line"`, `"outcomes"`, `"yesno"` or
#'   `"labelled"`), `period`, `prematch` and `live`.
#' @examples
#' \dontrun{
#' markets <- oa_markets("football")
#' subset(markets, live)
#' }
#' @export
oa_markets <- function(sport = NULL, api_key = NULL) {
  json <- oa_get("markets", list(sport = sport), oa_optional_key(api_key))
  oa_parse_markets(json)
}

oa_parse_markets <- function(json) {
  obj <- oa_json(json)
  by_sport <- if (!is.null(obj$markets)) {
    stats::setNames(list(obj$markets), obj$sport$slug %||% NA_character_)
  } else {
    obj
  }
  rows <- list()
  for (slug in names(by_sport)) {
    for (m in by_sport[[slug]]) {
      rows[[length(rows) + 1L]] <- c(list(sport = slug), lapply(m, oa_scalar))
    }
  }
  oa_bind_rows(rows, data.frame(
    sport = character(), name = character(), shape = character(),
    period = character(), prematch = logical(), live = logical(),
    stringsAsFactors = FALSE
  ))
}

#' List leagues for a sport
#'
#' @param sport Sport slug, e.g. `"football"`. See [oa_sports()].
#' @param all If `TRUE`, also include leagues without active events.
#' @inheritParams oa_sports
#' @return A data frame with one row per league (columns `name`, `slug` and
#'   `eventsCount`).
#' @examples
#' \dontrun{
#' leagues <- oa_leagues("football")
#' head(leagues)
#' }
#' @export
oa_leagues <- function(sport, all = FALSE, api_key = NULL) {
  oa_check_string(sport, "sport")
  oa_parse_df(oa_get("leagues", list(sport = sport, all = oa_flag(all)),
                     api_key))
}

#' List events for a sport
#'
#' Returns upcoming, live, settled or cancelled events. When `to` is omitted
#' the API returns events within the next 14 days, hard-capped at 5000 rows
#' per response; use `limit` and `skip` to paginate.
#'
#' Scores are flattened into columns such as `scores.home`, `scores.away`,
#' `scores.periods.p1.home` and `scores.periods.ft.home`. Period keys are
#' `p1` to `pN` (each period on its own), `ft` (end of regulation), `ot`
#' (after overtime) and `ap` (after penalties).
#'
#' @param sport Sport slug, e.g. `"football"`. See [oa_sports()].
#' @param league Optional league slug, e.g. `"england-premier-league"`.
#' @param status Optional statuses to include: any of `"pending"`, `"live"`,
#'   `"settled"`, `"cancelled"`. A vector is collapsed to a comma-separated
#'   list.
#' @param from,to Optional RFC 3339 date-times bounding the event window,
#'   e.g. `"2026-09-01T00:00:00Z"`, or `POSIXct`/`Date` values.
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
  oa_check_string(sport, "sport")
  query <- list(
    sport = sport,
    league = league,
    status = oa_collapse(status),
    from = oa_time(from),
    to = oa_time(to),
    limit = limit,
    skip = skip,
    bookmaker = bookmaker,
    participantId = participant_id
  )
  oa_parse_df(oa_get("events", query, api_key))
}

#' List live events
#'
#' Returns events that are in play now, with their current scores and, when
#' available, the match clock (`clock.minute`, `clock.period`,
#' `clock.running`, ...).
#'
#' @param sport Optional sport slug, e.g. `"football"`.
#' @inheritParams oa_sports
#' @return A data frame with one row per live event.
#' @examples
#' \dontrun{
#' live <- oa_live_events("football")
#' live[, c("id", "home", "away", "scores.home", "scores.away")]
#' }
#' @export
oa_live_events <- function(sport = NULL, api_key = NULL) {
  oa_parse_df(oa_get("events/live", list(sport = sport), api_key))
}

#' Search events
#'
#' Searches events by team or league name. Returns up to 10 events ranked by
#' relevance. Matching is case- and accent-insensitive.
#'
#' @param query Search term, at least 3 characters.
#' @param status `"upcoming"` (the API default) searches events that have
#'   not started; `"live"` searches in-play events.
#' @inheritParams oa_sports
#' @return A data frame with one row per matching event.
#' @examples
#' \dontrun{
#' oa_search_events("Liverpool")
#' oa_search_events("Liverpool", status = "live")
#' }
#' @export
oa_search_events <- function(query, status = NULL, api_key = NULL) {
  oa_check_string(query, "query")
  if (nchar(query) < 3) {
    stop("`query` must be at least 3 characters.", call. = FALSE)
  }
  oa_parse_df(oa_get("events/search", list(query = query, status = status),
                     api_key))
}

#' Get one event
#'
#' @param event_id Event id, from [oa_events()].
#' @inheritParams oa_sports
#' @return A one-row data frame describing the event.
#' @examples
#' \dontrun{
#' oa_event(123456)
#' }
#' @export
oa_event <- function(event_id, api_key = NULL) {
  stopifnot(length(event_id) == 1)
  oa_parse_object_df(oa_get(oa_id_path("events", event_id), api_key = api_key))
}

oa_id_path <- function(prefix, id) {
  paste0(prefix, "/", utils::URLencode(as.character(id), reserved = TRUE))
}

#' List participants (teams and players)
#'
#' @param sport Sport slug, e.g. `"football"`.
#' @param search Optional name filter, e.g. `"Chelsea"`.
#' @inheritParams oa_sports
#' @return A data frame with one row per participant (columns `id`, `name`,
#'   `sport` and, for esports, `game` and `shortCode`).
#' @examples
#' \dontrun{
#' oa_participants("football", search = "Chelsea")
#' }
#' @export
oa_participants <- function(sport, search = NULL, api_key = NULL) {
  oa_check_string(sport, "sport")
  oa_parse_df(oa_get("participants", list(sport = sport, search = search),
                     api_key))
}

#' @rdname oa_participants
#' @param participant_id Participant id, for example `homeId` or `awayId`
#'   from [oa_events()].
#' @export
oa_participant <- function(participant_id, api_key = NULL) {
  stopifnot(length(participant_id) == 1)
  oa_parse_object_df(oa_get(oa_id_path("participants", participant_id),
                            api_key = api_key))
}
