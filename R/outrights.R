#' List outright tournaments
#'
#' Returns outright (futures) tournaments for a sport, sorted by date. Golf
#' is the first sport: PGA Tour, DP World Tour, LIV Golf and the majors.
#' Outrights are a paid add-on; a key without it gets an error that carries
#' the API's message and a link to add it.
#'
#' @param sport Sport slug, e.g. `"golf"`.
#' @param league Optional league slug: `"pga-tour"`, `"dp-world-tour"`,
#'   `"liv-golf"` or `"majors"`.
#' @param status Optional statuses: any of `"pending"`, `"live"`,
#'   `"settled"`, `"cancelled"`. The API default is pending and live.
#' @inheritParams oa_sports
#' @return A data frame with one row per tournament and columns `id`,
#'   `name`, `date`, `status`, `sport.*`, `league.*` and `bookmakerCount`
#'   (how many bookmakers you can request price it now).
#' @examples
#' \dontrun{
#' golf <- oa_outrights("golf", league = "pga-tour")
#' subset(golf, bookmakerCount > 0)
#' }
#' @export
oa_outrights <- function(sport = "golf", league = NULL, status = NULL,
                         api_key = NULL) {
  oa_check_string(sport, "sport")
  query <- list(sport = sport, league = league, status = oa_collapse(status))
  oa_parse_df(oa_get("outrights", query, api_key))
}

#' Get outright odds for a tournament
#'
#' Golf markets are `"Winner"`, `"Top 5"`, `"Top 5 (Incl. Ties)"`,
#' `"Top 10"`, `"Top 10 (Incl. Ties)"`, `"Top 20"`, `"Top 20 (Incl. Ties)"`
#' and `"Make Cut"`. Top N settles a boundary tie under the bookmaker's
#' dead-heat rules; Top N (Incl. Ties) pays a boundary tie in full.
#'
#' @param outright_id Tournament id, from [oa_outrights()].
#' @param bookmakers Character vector of bookmaker names (max 30).
#' @inheritParams oa_sports
#' @return A data frame with one row per bookmaker, market and runner, with
#'   columns `outrightId`, `bookmaker`, `market`, `updatedAt`, `label`
#'   (golfer), `participantId` and the prices: `odds` (and `layOdds` on
#'   exchanges) for Winner and Top N, `yes`/`no` for Make Cut. Prices are
#'   decimal odds as strings.
#' @examples
#' \dontrun{
#' golf <- oa_outrights("golf")
#' odds <- oa_outright_odds(golf$id[1], c("DraftKings", "Betfair Exchange"))
#' subset(odds, market == "Winner")
#' }
#' @export
oa_outright_odds <- function(outright_id, bookmakers, api_key = NULL) {
  stopifnot(length(outright_id) == 1, length(bookmakers) >= 1)
  oa_check_max(bookmakers, 30, "bookmakers")
  query <- list(id = outright_id, bookmakers = oa_collapse(bookmakers))
  oa_parse_outright_odds(oa_get("outrights/odds", query, api_key))
}

oa_parse_outright_odds <- function(json) {
  obj <- oa_json(json)
  rows <- list()
  for (bk in names(obj$bookmakers)) {
    for (market in obj$bookmakers[[bk]]) {
      for (runner in market$odds) {
        rows[[length(rows) + 1L]] <- c(
          list(
            outrightId = obj$id,
            bookmaker = bk,
            market = market$name,
            updatedAt = market$updatedAt %||% NA_character_
          ),
          lapply(runner, oa_scalar)
        )
      }
    }
  }
  oa_bind_rows(rows, data.frame(
    outrightId = numeric(), bookmaker = character(), market = character(),
    updatedAt = character(), label = character(), stringsAsFactors = FALSE
  ))
}

#' Get outright results for a tournament
#'
#' @param outright_id Tournament id, from [oa_outrights()].
#' @inheritParams oa_sports
#' @return A data frame with one row per golfer and columns `label`,
#'   `participantId`, `position` (shared by tied golfers, `NA` for cut,
#'   withdrawn or disqualified), `tied`, `madeCut` and `status`. It is empty
#'   until the tournament is settled. Attribute `winners` is a named list of
#'   the participant ids that won each market, and attribute `status` holds
#'   the tournament status.
#' @examples
#' \dontrun{
#' res <- oa_outright_results(1001)
#' attr(res, "winners")[["Top 10"]]
#' }
#' @export
oa_outright_results <- function(outright_id, api_key = NULL) {
  stopifnot(length(outright_id) == 1)
  obj <- oa_json(oa_get("outrights/results", list(id = outright_id), api_key))
  out <- oa_list_df(obj$results)
  attr(out, "winners") <- lapply(obj$winners, function(x) unlist(x))
  attr(out, "status") <- obj$status
  out
}
