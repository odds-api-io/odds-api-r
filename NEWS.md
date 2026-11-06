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
