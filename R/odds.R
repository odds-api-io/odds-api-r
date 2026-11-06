#' Get odds for an event
#'
#' Fetches odds for one event from up to 30 bookmakers and returns them as a
#' tidy data frame with one row per price line.
#'
#' @param event_id Event id, from [oa_events()].
#' @param bookmakers Character vector of bookmaker names (max 30), e.g.
#'   `c("Bet365", "SingBet")`. Required by the API.
#' @param markets Optional character vector of market names to keep (max
#'   40), e.g. `c("ML", "Spread", "Totals")`. Matching is case-insensitive
#'   but must be the full name; see [oa_markets()]. All markets are returned
#'   when omitted.
#' @inheritParams oa_sports
#' @return A data frame with one row per odds line, with columns `eventId`,
#'   `bookmaker`, `market`, `updatedAt` and the price columns present in the
#'   response (such as `home`, `draw`, `away`, `hdp`, `over`, `under`).
#'   Prices are decimal odds as strings, as served by the API.
#' @examples
#' \dontrun{
#' odds <- oa_odds(123456, bookmakers = c("Bet365", "SingBet"),
#'                 markets = c("ML", "Totals"))
#' subset(odds, market == "ML")
#' }
#' @export
oa_odds <- function(event_id, bookmakers, markets = NULL, api_key = NULL) {
  stopifnot(length(event_id) == 1, length(bookmakers) >= 1)
  oa_check_max(bookmakers, 30, "bookmakers")
  oa_check_max(markets, 40, "markets")
  query <- list(
    eventId = event_id,
    bookmakers = oa_collapse(bookmakers),
    markets = oa_collapse(markets)
  )
  oa_parse_odds(oa_get("odds", query, api_key))
}

#' Get odds for several events
#'
#' Fetches odds for up to 10 events in one request (counted as one request
#' against your rate limit).
#'
#' @param event_ids Vector of up to 10 event ids.
#' @inheritParams oa_odds
#' @return A tidy data frame in the same format as [oa_odds()], with rows
#'   for every event.
#' @examples
#' \dontrun{
#' events <- oa_events("football", league = "england-premier-league",
#'                     limit = 10)
#' odds <- oa_multi_odds(events$id, bookmakers = "Bet365", markets = "ML")
#' }
#' @export
oa_multi_odds <- function(event_ids, bookmakers, markets = NULL,
                          api_key = NULL) {
  stopifnot(length(event_ids) >= 1, length(bookmakers) >= 1)
  oa_check_max(event_ids, 10, "event_ids")
  oa_check_max(bookmakers, 30, "bookmakers")
  oa_check_max(markets, 40, "markets")
  query <- list(
    eventIds = oa_collapse(event_ids),
    bookmakers = oa_collapse(bookmakers),
    markets = oa_collapse(markets)
  )
  oa_parse_odds_list(oa_get("odds/multi", query, api_key))
}

#' Get odds updated since a timestamp
#'
#' Returns a delta: only the markets of one bookmaker that changed since
#' `since` (at most 90 seconds ago). Pass the `next_since` attribute of the
#' result as `since` on your next call to continue with no gap. Updates can
#' arrive twice, so merge by `eventId`, `bookmaker` and `market`. A market
#' absent from a response has not changed.
#'
#' @param since Unix timestamp in seconds, or a `POSIXct`, at most 90
#'   seconds old.
#' @param bookmaker Bookmaker name, e.g. `"Bet365"`.
#' @param sport Sport slug, e.g. `"football"`.
#' @param markets Character vector of market names (required, max 40), e.g.
#'   `c("ML", "Spread", "Totals")`.
#' @param limit Optional maximum number of events (1 to 10000).
#' @inheritParams oa_sports
#' @return A tidy data frame in the same format as [oa_odds()], with
#'   attributes `next_since` (pass it as `since` on the next call) and
#'   `server_time`, both Unix seconds.
#' @examples
#' \dontrun{
#' upd <- oa_odds_updated(Sys.time() - 20, "Bet365", "football",
#'                        markets = c("ML", "Totals"))
#' upd2 <- oa_odds_updated(attr(upd, "next_since"), "Bet365", "football",
#'                         markets = c("ML", "Totals"))
#' }
#' @export
oa_odds_updated <- function(since, bookmaker, sport, markets, limit = NULL,
                            api_key = NULL) {
  oa_check_string(bookmaker, "bookmaker")
  oa_check_string(sport, "sport")
  if (missing(markets) || !length(markets)) {
    stop("`markets` is required, e.g. c(\"ML\", \"Spread\", \"Totals\"). ",
         "See oa_markets() for names.", call. = FALSE)
  }
  oa_check_max(markets, 40, "markets")
  if (inherits(since, "POSIXt")) {
    since <- floor(as.numeric(since))
  }
  query <- list(
    since = format(since, scientific = FALSE),
    bookmaker = bookmaker,
    sport = sport,
    markets = oa_collapse(markets),
    limit = limit
  )
  resp <- oa_perform("odds/updated", query, api_key)
  out <- oa_parse_odds_list(httr2::resp_body_string(resp))
  attr(out, "next_since") <- oa_header_num(resp, "X-Next-Since")
  attr(out, "server_time") <- oa_header_num(resp, "X-Server-Time")
  out
}

#' Get odds movements for a market
#'
#' Returns the recorded pre-match price history of one market at one
#' bookmaker. History is kept for a fixed set of bookmakers; the API returns
#' a 404 that names the markets or lines with history when the request has
#' none.
#'
#' @param event_id Event id.
#' @param bookmaker Bookmaker name, case-insensitive, e.g. `"Bet365"`.
#' @param market Market name, case-insensitive, e.g. `"ML"`, `"Spread"`,
#'   `"Totals"`.
#' @param market_line Optional handicap line, e.g. `2.5`. Leave it out for
#'   the main line of `Spread` and `Totals`, or for markets without a line.
#' @inheritParams oa_sports
#' @return A data frame with one row per recorded price, oldest first, with
#'   the price columns (`home`, `draw`, `away`; on Totals the over price is
#'   in `home` and the under price in `away`), `hdp` where the market has a
#'   line, `timestamp` (Unix milliseconds) and `time` (`POSIXct`, UTC).
#'   Attributes `opening` and `latest` hold the opening and latest prices.
#' @examples
#' \dontrun{
#' mv <- oa_odds_movements(123456, "Bet365", "ML")
#' plot(mv$time, mv$home, type = "s")
#' }
#' @export
oa_odds_movements <- function(event_id, bookmaker, market, market_line = NULL,
                              api_key = NULL) {
  stopifnot(length(event_id) == 1)
  oa_check_string(bookmaker, "bookmaker")
  oa_check_string(market, "market")
  query <- list(
    eventId = event_id,
    bookmaker = bookmaker,
    market = market,
    marketLine = market_line
  )
  oa_parse_movements(oa_get("odds/movements", query, api_key))
}

oa_parse_movements <- function(json) {
  obj <- oa_json(json)
  rows <- lapply(obj$movements, function(m) lapply(m, oa_scalar))
  out <- oa_bind_rows(rows, data.frame(timestamp = numeric()))
  out$time <- as.POSIXct(as.numeric(out$timestamp) / 1000,
                         origin = "1970-01-01", tz = "UTC")
  attr(out, "opening") <- obj$opening
  attr(out, "latest") <- obj$latest
  out
}

#' Get an odds summary across bookmakers
#'
#' Returns the best, average, median, worst and margin-free fair price per
#' market line across bookmakers, for odds comparison. Available on Growth
#' plans and above; other plans receive a fixed sample response, flagged by
#' the `demo` attribute.
#'
#' @param event_id Event id.
#' @param event_ids Vector of up to 10 event ids.
#' @param markets Optional market names (max 20). The API default is
#'   `c("ML", "Spread", "Totals")`.
#' @param lines `"main"` (the API default) for each market's main line, or
#'   `"all"` for every line priced by at least 3 bookmakers.
#' @param bookmakers Optional bookmakers to restrict the summary to; each
#'   must be in your selected bookmakers.
#' @param include_exchanges If `TRUE`, add the best exchange back price per
#'   outcome as `exchange*` columns.
#' @inheritParams oa_sports
#' @return A data frame with one row per event, market, line and outcome,
#'   with columns such as `eventId`, `market`, `hdp`, `main`, `books`,
#'   `outcome`, `best`, `bestBookmakers`, `average`, `median`, `worst` and
#'   `fair`. Attribute `demo` is `TRUE` when the response is the sample.
#' @examples
#' \dontrun{
#' s <- oa_odds_summary(123456, markets = c("ML", "Totals"))
#' s[, c("market", "hdp", "outcome", "best", "fair")]
#' }
#' @export
oa_odds_summary <- function(event_id, markets = NULL, lines = NULL,
                            bookmakers = NULL, include_exchanges = FALSE,
                            api_key = NULL) {
  stopifnot(length(event_id) == 1)
  query <- oa_summary_query(markets, lines, bookmakers, include_exchanges)
  json <- oa_get("odds/summary", c(list(eventId = event_id), query), api_key)
  obj <- oa_json(json)
  oa_summary_df(list(obj))
}

#' @rdname oa_odds_summary
#' @export
oa_odds_summary_multi <- function(event_ids, markets = NULL, lines = NULL,
                                  bookmakers = NULL,
                                  include_exchanges = FALSE, api_key = NULL) {
  stopifnot(length(event_ids) >= 1)
  oa_check_max(event_ids, 10, "event_ids")
  query <- oa_summary_query(markets, lines, bookmakers, include_exchanges)
  json <- oa_get("odds/summary/multi",
                 c(list(eventIds = oa_collapse(event_ids)), query), api_key)
  oa_summary_df(oa_json(json))
}

oa_summary_query <- function(markets, lines, bookmakers, include_exchanges) {
  oa_check_max(markets, 20, "markets")
  if (!is.null(lines)) {
    lines <- match.arg(lines, c("main", "all"))
  }
  list(
    markets = oa_collapse(markets),
    lines = lines,
    bookmakers = oa_collapse(bookmakers),
    includeExchanges = oa_flag(include_exchanges)
  )
}

oa_prefix <- function(prefix, x) {
  if (!length(x)) {
    return(list())
  }
  stats::setNames(
    lapply(x, oa_scalar),
    paste0(prefix, toupper(substr(names(x), 1, 1)), substring(names(x), 2))
  )
}

oa_summary_df <- function(events) {
  rows <- list()
  for (ev in events) {
    head <- list(
      eventId = oa_scalar(ev$eventId), home = oa_scalar(ev$home),
      away = oa_scalar(ev$away), date = oa_scalar(ev$date)
    )
    for (m in ev$markets) {
      for (ln in m$lines) {
        line <- list(
          market = m$name, hdp = oa_scalar(ln$hdp), main = oa_scalar(ln$main),
          books = oa_scalar(ln$books), fairBooks = oa_scalar(ln$fairBooks)
        )
        line <- c(line, oa_prefix("payout", ln$payout))
        for (side in names(ln$outcomes)) {
          o <- ln$outcomes[[side]]
          o$bestBookmakers <- paste(unlist(o$bestBookmakers), collapse = ",")
          rows[[length(rows) + 1L]] <- c(
            head, line, list(outcome = side), lapply(o, oa_scalar),
            oa_prefix("exchange", ln$exchanges[[side]])
          )
        }
      }
    }
  }
  out <- oa_bind_rows(rows, data.frame(
    eventId = character(), market = character(), outcome = character(),
    stringsAsFactors = FALSE
  ))
  attr(out, "demo") <- any(vapply(events, function(e) isTRUE(e$demo),
                                  logical(1)))
  out
}

#' Get value bets
#'
#' Returns prices at one bookmaker that are above a fair-odds estimate
#' calculated from the market consensus.
#'
#' @param bookmaker Bookmaker name, e.g. `"Bet365"`. Required by the API.
#' @param sport Optional sport slug filter, e.g. `"baseball"`.
#' @param league Optional league slug filter; requires `sport` to be set.
#' @param include_event_details If `TRUE`, event information (teams, date,
#'   sport, league) is included in the result.
#' @inheritParams oa_sports
#' @return A data frame with one row per value bet, including the bet side,
#'   expected value and bookmaker odds. `expectedValue` is bookmaker odds /
#'   fair odds x 100: 103.42 is a 3.42% edge, so the edge in percent is
#'   `expectedValue - 100`. Values below 100 are a negative edge.
#' @examples
#' \dontrun{
#' vb <- oa_value_bets("Bet365", include_event_details = TRUE)
#' # Bets with at least a 3% edge
#' vb[vb$expectedValue - 100 >= 3, ]
#' }
#' @export
oa_value_bets <- function(bookmaker, sport = NULL, league = NULL,
                          include_event_details = FALSE, api_key = NULL) {
  oa_check_string(bookmaker, "bookmaker")
  oa_check_league(league, sport)
  query <- list(
    bookmaker = bookmaker,
    sport = sport,
    league = league,
    includeEventDetails = oa_flag(include_event_details)
  )
  oa_parse_df(oa_get("value-bets", query, api_key))
}

oa_check_league <- function(league, sport) {
  if (!is.null(league) && is.null(sport)) {
    stop("`league` requires `sport` to also be set.", call. = FALSE)
  }
}

#' Get arbitrage bets
#'
#' Returns pre-match price gaps across the given bookmakers where the best
#' prices for every outcome imply a total probability under 100%, with the
#' legs and a stake split for each. Prices move quickly and bookmakers can
#' void or limit bets, so check every leg before acting on one.
#'
#' @param bookmakers Character vector of bookmaker names to compare.
#'   Required by the API.
#' @param sport Optional sport slug, e.g. `"football"`.
#' @param league Optional league slug; requires `sport`.
#' @param markets Optional market names to keep (max 40), e.g.
#'   `c("ML", "Totals")`.
#' @param min_profit Optional minimum `profitMargin` in percent (0 to 100),
#'   e.g. `1.5`.
#' @param all_combinations If `TRUE`, return every bookmaker combination per
#'   event, market and line instead of only the best one.
#' @param limit Optional maximum number of results (default 50, max 500).
#' @param include_event_details If `TRUE`, event information is included.
#' @inheritParams oa_sports
#' @return A data frame with one row per arbitrage opportunity. The `legs`
#'   and `optimalStakes` columns are list-columns of data frames.
#' @examples
#' \dontrun{
#' arbs <- oa_arbitrage_bets(c("Bet365", "SingBet"), sport = "football",
#'                           min_profit = 1, include_event_details = TRUE)
#' arbs[order(-arbs$profitMargin), ]
#' }
#' @export
oa_arbitrage_bets <- function(bookmakers, sport = NULL, league = NULL,
                              markets = NULL, min_profit = NULL,
                              all_combinations = FALSE, limit = NULL,
                              include_event_details = FALSE, api_key = NULL) {
  stopifnot(length(bookmakers) >= 1)
  oa_check_league(league, sport)
  oa_check_max(markets, 40, "markets")
  query <- list(
    bookmakers = oa_collapse(bookmakers),
    sport = sport,
    league = league,
    markets = oa_collapse(markets),
    minProfit = min_profit,
    allCombinations = oa_flag(all_combinations),
    limit = limit,
    includeEventDetails = oa_flag(include_event_details)
  )
  oa_parse_df(oa_get("arbitrage-bets", query, api_key))
}

#' Get dropping odds
#'
#' Returns prices at sharp bookmakers that have dropped, sorted by the size
#' of the drop, the latest movement or kickoff. Requires a paid plan.
#'
#' @param sport Optional sport slug, e.g. `"football"`.
#' @param league Optional league slug; requires `sport`. Cannot be combined
#'   with `leagues`.
#' @param leagues Optional character vector of league slugs.
#' @param markets Optional market names, e.g. `c("Spread", "Totals")`.
#' @param time_window Window for the drop: `"opening"` (the API default),
#'   `"12h"`, `"24h"` or `"48h"`.
#' @param sort `"drop"` (the API default), `"recent"` or `"kickoff"`.
#' @param min_drop Optional minimum drop in percent.
#' @param limit Optional results per page (default 50, max 200).
#' @param page Optional page number, starting at 1.
#' @param include_event_details If `TRUE`, event information is included.
#' @inheritParams oa_sports
#' @return A data frame with one row per dropping price. Attributes
#'   `total_count`, `page` and `per_page` describe the pagination.
#' @examples
#' \dontrun{
#' drops <- oa_dropping_odds("football", min_drop = 10,
#'                           include_event_details = TRUE)
#' attr(drops, "total_count")
#' }
#' @export
oa_dropping_odds <- function(sport = NULL, league = NULL, leagues = NULL,
                             markets = NULL, time_window = NULL, sort = NULL,
                             min_drop = NULL, limit = NULL, page = NULL,
                             include_event_details = FALSE, api_key = NULL) {
  oa_check_league(league, sport)
  if (!is.null(league) && !is.null(leagues)) {
    stop("Use either `league` or `leagues`, not both.", call. = FALSE)
  }
  if (!is.null(time_window)) {
    time_window <- match.arg(time_window, c("opening", "12h", "24h", "48h"))
  }
  if (!is.null(sort)) {
    sort <- match.arg(sort, c("drop", "recent", "kickoff"))
  }
  query <- list(
    sport = sport,
    league = league,
    leagues = oa_collapse(leagues),
    markets = oa_collapse(markets),
    timeWindow = time_window,
    sort = sort,
    minDrop = min_drop,
    limit = limit,
    page = page,
    includeEventDetails = oa_flag(include_event_details)
  )
  resp <- oa_perform("dropping-odds", query, api_key)
  out <- oa_parse_df(httr2::resp_body_string(resp))
  attr(out, "total_count") <- oa_header_num(resp, "X-Total-Count")
  attr(out, "page") <- oa_header_num(resp, "X-Page")
  attr(out, "per_page") <- oa_header_num(resp, "X-Per-Page")
  out
}
