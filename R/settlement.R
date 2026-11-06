#' Get settlement records for events
#'
#' Returns the result status of up to 50 events. `final` is `TRUE` once you
#' can stop polling. Requires a paid plan.
#'
#' @param event_ids Vector of 1 to 50 event ids.
#' @inheritParams oa_sports
#' @return A data frame with one row per known event and columns `eventId`,
#'   `sport`, `status` (`"pending"`, `"live"`, `"awaiting_result"`,
#'   `"settled"` or `"cancelled"`), `final`, `settledAt` and, for settled
#'   events, score columns (`scores.home`, `scores.away` and period columns
#'   with keys `p1` to `pN`, `ft`, `ot` and `ap`). Attribute `not_found`
#'   lists ids the API has never seen.
#' @examples
#' \dontrun{
#' s <- oa_settlements(c(74919888, 73101436))
#' attr(s, "not_found")
#' }
#' @export
oa_settlements <- function(event_ids, api_key = NULL) {
  stopifnot(length(event_ids) >= 1)
  oa_check_max(event_ids, 50, "event_ids")
  json <- oa_get("settlements", list(eventIds = oa_collapse(event_ids)),
                 api_key)
  obj <- oa_json(json)
  out <- oa_list_df(obj$settlements)
  attr(out, "not_found") <- as.character(unlist(obj$notFound))
  out
}

#' Grade bet selections
#'
#' Grades up to 50 bet selections against final results. One bad selection
#' never fails the batch: it comes back as `ungradeable` with a reason.
#' Requires a paid plan.
#'
#' @param selections A data frame (or a list of lists) with one row per
#'   selection. Columns: `eventId` and `market` (required), plus `side`,
#'   `hdp`, `label`, `tieRule` and `ref` where needed. Use market names,
#'   lines and labels exactly as served by [oa_odds()]; handicaps are stated
#'   for the home side. `NA` values are left out of the request.
#' @inheritParams oa_sports
#' @return A data frame with one row per selection, in the order sent, with
#'   columns `ref`, `eventId`, `result` (`"won"`, `"lost"`, `"void"`,
#'   `"half_won"`, `"half_lost"`, `"pending"` or `"ungradeable"`), `reason`,
#'   `final` and, for score markets, `basis.*` columns.
#' @examples
#' \dontrun{
#' bets <- data.frame(
#'   ref = c("bet-1", "bet-2"),
#'   eventId = c(74919888, 74919888),
#'   market = c("ML", "Spread"),
#'   side = c("home", "away"),
#'   hdp = c(NA, -0.75)
#' )
#' oa_grade(bets)
#' }
#' @export
oa_grade <- function(selections, api_key = NULL) {
  sel <- oa_selections(selections)
  if (!length(sel)) {
    stop("`selections` must hold at least one selection.", call. = FALSE)
  }
  oa_check_max(sel, 50, "selections")
  resp <- oa_perform("settlements/grade", api_key = api_key, method = "POST",
                     body = list(selections = sel))
  oa_list_df(oa_json(httr2::resp_body_string(resp))$grades)
}

oa_selections <- function(selections) {
  if (is.data.frame(selections)) {
    selections <- lapply(seq_len(nrow(selections)), function(i) {
      as.list(selections[i, , drop = FALSE])
    })
  }
  lapply(selections, function(s) {
    s <- lapply(s, function(v) if (is.factor(v)) as.character(v) else v)
    keep <- vapply(s, function(v) length(v) == 1 && !is.na(v), logical(1))
    s <- s[keep]
    for (req in c("eventId", "market")) {
      if (is.null(s[[req]])) {
        stop("Every selection needs `", req, "`.", call. = FALSE)
      }
    }
    s
  })
}
