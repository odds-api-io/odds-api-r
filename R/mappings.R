#' Get native id mappings
#'
#' Links events and selections to a bookmaker's own event, market and
#' outcome ids (for example Polymarket condition and token ids, or Kalshi
#' tickers). Look up by `event_ids`, by `market_ids` (optionally with
#' `outcome_id`), or leave all three out for a bulk sync of upcoming and
#' live events, paged with `cursor`. Requires a paid plan, and the
#' bookmaker must be in your selection.
#'
#' @param bookmaker Bookmaker name, e.g. `"Polymarket"` or `"Kalshi"`.
#' @param event_ids Optional vector of up to 10 event ids.
#' @param market_ids Optional vector of up to 10 bookmaker market ids.
#' @param outcome_id Optional bookmaker outcome id.
#' @param market Optional exact market name as served by [oa_odds()]
#'   (case-sensitive).
#' @param active Optional `TRUE` for outcomes the bookmaker still lists,
#'   `FALSE` for withdrawn ones. Both when omitted.
#' @param sport,league Optional bulk sync filters; `league` requires
#'   `sport`.
#' @param limit Optional rows per page in bulk sync (1 to 5000).
#' @param cursor Optional `next_cursor` attribute from the previous page.
#' @inheritParams oa_sports
#' @return A data frame with one row per mapping, including `eventId`,
#'   `market`, `hdp`, `side`, `nativeEventId`, `marketId`, `outcomeId` and
#'   `active`. Attribute `next_cursor` is `NULL` on the last page.
#' @examples
#' \dontrun{
#' oa_mappings("Polymarket", event_ids = 71515842)
#'
#' page <- oa_mappings("Kalshi", sport = "american-football", limit = 5000)
#' while (!is.null(attr(page, "next_cursor"))) {
#'   page <- oa_mappings("Kalshi", sport = "american-football", limit = 5000,
#'                       cursor = attr(page, "next_cursor"))
#' }
#' }
#' @export
oa_mappings <- function(bookmaker, event_ids = NULL, market_ids = NULL,
                        outcome_id = NULL, market = NULL, active = NULL,
                        sport = NULL, league = NULL, limit = NULL,
                        cursor = NULL, api_key = NULL) {
  oa_check_string(bookmaker, "bookmaker")
  oa_check_max(event_ids, 10, "event_ids")
  oa_check_max(market_ids, 10, "market_ids")
  oa_check_league(league, sport)
  if (!is.null(event_ids) && (!is.null(market_ids) || !is.null(outcome_id))) {
    stop("`event_ids` cannot be combined with `market_ids` or `outcome_id`.",
         call. = FALSE)
  }
  query <- list(
    bookmaker = bookmaker,
    eventId = if (length(event_ids) == 1) event_ids else NULL,
    eventIds = if (length(event_ids) > 1) oa_collapse(event_ids) else NULL,
    marketId = if (length(market_ids) == 1) market_ids else NULL,
    marketIds = if (length(market_ids) > 1) oa_collapse(market_ids) else NULL,
    outcomeId = outcome_id,
    market = market,
    active = if (is.null(active)) NULL else tolower(as.character(active)),
    sport = sport,
    league = league,
    limit = limit,
    cursor = cursor
  )
  obj <- oa_json(oa_get("mappings", query, api_key))
  out <- oa_list_df(obj$data)
  attr(out, "next_cursor") <- obj$nextCursor
  out
}
