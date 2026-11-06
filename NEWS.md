# oddsapiio 0.2.0

Covers the full current Odds-API.io v3 REST API. This release also includes the 0.1.2 changes, which never reached CRAN: documentation and errors point to paid plans at odds-api.io/pricing (new free keys are paused) and give the current bookmaker count (450+).

## New functions

* Catalogue: `oa_markets()`, `oa_selected_bookmakers()`, `oa_select_bookmakers()`, `oa_clear_selected_bookmakers()`.
* Events and participants: `oa_live_events()`, `oa_search_events()`, `oa_event()`, `oa_participants()`, `oa_participant()`.
* Odds: `oa_multi_odds()`, `oa_odds_updated()` (delta feed; `markets` required, max 40; `next_since` attribute from `X-Next-Since`), `oa_odds_movements()`, `oa_odds_summary()`, `oa_odds_summary_multi()`, `oa_dropping_odds()`.
* Historical: `oa_historical_events()` (league required, at most 31 days), `oa_historical_search()`, `oa_historical_odds()`, `oa_closing_lines()` (up to 10 leagues and 366 days).
* Settlement: `oa_settlements()` and `oa_grade()`.
* Outrights: `oa_outrights()`, `oa_outright_odds()` (golf Winner, Top 5/10/20 with and without ties, Make Cut) and `oa_outright_results()`.
* `oa_mappings()` for native bookmaker event, market and outcome ids, with cursor paging.

## Changes

* `oa_odds()` gains `markets`. `oa_leagues()` gains `all`. `oa_arbitrage_bets()` gains `sport`, `league`, `markets`, `min_profit` and `all_combinations`.
* `oa_events()` documents the `cancelled` status and accepts `POSIXct`/`Date` for `from` and `to`.
* Examples and tests use current market names (`ML`, `Spread`, `Totals`) and period keys (`p1`..`pN`, `ft`, `ot`, `ap`). The legacy keys `period1` and `overtime` stop being served on 12 November 2026.
* Errors now lead with the API's own message (for example the Outrights add-on notice with its billing link), followed by a hint for 401, 403 and 429.
* Requests are retried up to 3 times on 502, 503 and 504.
* Requires httr2 >= 1.1.0 and R >= 4.0.0.

# oddsapiio 0.1.2

* Documentation and error messages now point to paid plans at odds-api.io/pricing (new free keys are paused) and give the current bookmaker count (450+).

# oddsapiio 0.1.1

* Fix invalid file URI in README (CRAN review feedback).

# oddsapiio 0.1.0

* Initial release.
* Functions covering the core Odds-API.io v3 endpoints: `oa_sports()`,
  `oa_bookmakers()`, `oa_leagues()`, `oa_events()`, `oa_odds()`,
  `oa_value_bets()` and `oa_arbitrage_bets()`.
* Tidy data frame output, API key via the `ODDS_API_KEY` environment
  variable, and helpful errors for authentication and rate limit issues.
