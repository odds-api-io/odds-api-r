# oddsapiio

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/oddsapiio)](https://CRAN.R-project.org/package=oddsapiio)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/license/mit)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22836647.svg)](https://doi.org/10.5281/zenodo.22836647)
<!-- badges: end -->

Official R client for [Odds-API.io](https://odds-api.io), a real-time sports betting odds API covering 450+ bookmakers across 34 sports. Every function returns a tidy data frame.

## Quick start

```r
install.packages("oddsapiio")
library(oddsapiio)

Sys.setenv(ODDS_API_KEY = "your-key")  # get a key at https://odds-api.io/pricing?ref=cran

events <- oa_events("football", league = "england-premier-league")
odds <- oa_odds(events$id[1], bookmakers = c("Bet365", "SingBet"),
                markets = c("ML", "Spread", "Totals"))
subset(odds, market == "ML")
#>   eventId bookmaker market            updatedAt home draw away
#> 1  123456    Bet365     ML 2026-08-14T10:30:00Z 2.10 3.40 3.20
```

An API key from a paid plan is required: see [odds-api.io/pricing](https://odds-api.io/pricing?ref=cran). Set it as the `ODDS_API_KEY` environment variable (for example in `.Renviron`), or pass `api_key` to any function.

## Features

- Pre-match and live odds from 450+ bookmakers, one row per price line, filtered to the markets you need
- Odds for up to 10 events per request, and a delta feed of changed odds (`oa_odds_updated()`)
- Odds movements (price history) and an odds summary with best, average and fair prices across bookmakers
- Value bets, arbitrage bets and dropping odds
- Historical events, historical odds and closing lines for a whole season in one sweep, for closing line value and modelling
- Bet settlement and grading from final results
- Outrights (futures): golf tournaments, odds for Winner, Top 5/10/20 and Make Cut, and results
- Market catalogue, events, live events, search, participants and native bookmaker id mappings

## Installation

From CRAN:

```r
install.packages("oddsapiio")
```

Development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("odds-api-io/odds-api-r")
```

## Examples

```r
library(oddsapiio)

# Catalogue (no API key needed)
oa_sports()
oa_bookmakers()
oa_markets("football")

# Events
oa_leagues("football")
oa_live_events("football")
oa_search_events("Liverpool")

# Odds for several events, and changes since a timestamp
ids <- events$id[1:10]
multi <- oa_multi_odds(ids, bookmakers = "Bet365", markets = "ML")
upd <- oa_odds_updated(Sys.time() - 20, "Bet365", "football",
                       markets = c("ML", "Totals"))
attr(upd, "next_since")  # pass as `since` on the next call

# Price history and cross-bookmaker summary
oa_odds_movements(ids[1], "Bet365", "ML")
oa_odds_summary(ids[1], markets = c("ML", "Totals"))

# Value bets, arbitrage and dropping odds
oa_value_bets("Bet365", include_event_details = TRUE)
oa_arbitrage_bets(c("Bet365", "SingBet"), sport = "football", min_profit = 1)
oa_dropping_odds("football", min_drop = 10)

# Closing lines for a season
cl <- oa_closing_lines("football", "england-premier-league",
                       from = "2025-08-01T00:00:00Z",
                       to = "2026-05-31T23:59:59Z",
                       markets = "ML", bookmakers = c("Bet365", "SingBet"))

# Settlement and grading
oa_settlements(ids[1:3])
oa_grade(data.frame(eventId = ids[1], market = "Spread", side = "home",
                    hdp = -0.5))

# Outrights (golf, Outrights add-on)
golf <- oa_outrights("golf", league = "pga-tour")
oa_outright_odds(golf$id[1], c("DraftKings", "Betfair Exchange"))
oa_outright_results(golf$id[1])
```

Market names are the ones served by the API, such as `ML`, `Spread`, `Totals`, `Both Teams To Score` and `Totals HT`; `oa_markets()` lists them per sport. Period scores use the keys `p1` to `pN`, `ft` (regulation), `ot` (after overtime) and `ap` (after penalties).

## Functions

| Function | Endpoint |
| --- | --- |
| `oa_sports()` | `GET /v3/sports` |
| `oa_markets(sport)` | `GET /v3/markets` |
| `oa_bookmakers()` | `GET /v3/bookmakers` |
| `oa_selected_bookmakers()`, `oa_select_bookmakers()`, `oa_clear_selected_bookmakers()` | `/v3/bookmakers/selected` |
| `oa_leagues(sport, all)` | `GET /v3/leagues` |
| `oa_events(sport, ...)` | `GET /v3/events` |
| `oa_live_events(sport)` | `GET /v3/events/live` |
| `oa_search_events(query, status)` | `GET /v3/events/search` |
| `oa_event(event_id)` | `GET /v3/events/{id}` |
| `oa_participants(sport, search)`, `oa_participant(id)` | `GET /v3/participants` |
| `oa_odds(event_id, bookmakers, markets)` | `GET /v3/odds` |
| `oa_multi_odds(event_ids, bookmakers, markets)` | `GET /v3/odds/multi` |
| `oa_odds_updated(since, bookmaker, sport, markets)` | `GET /v3/odds/updated` |
| `oa_odds_movements(event_id, bookmaker, market)` | `GET /v3/odds/movements` |
| `oa_odds_summary()`, `oa_odds_summary_multi()` | `GET /v3/odds/summary` |
| `oa_value_bets(bookmaker, ...)` | `GET /v3/value-bets` |
| `oa_arbitrage_bets(bookmakers, ...)` | `GET /v3/arbitrage-bets` |
| `oa_dropping_odds(...)` | `GET /v3/dropping-odds` |
| `oa_historical_events(sport, league, from, to)` | `GET /v3/historical/events` |
| `oa_historical_search(query, ...)` | `GET /v3/historical/events/search` |
| `oa_historical_odds(event_id, bookmakers, markets)` | `GET /v3/historical/odds` |
| `oa_closing_lines(sport, leagues, from, to, markets, bookmakers)` | `GET /v3/historical/closing-lines` |
| `oa_settlements(event_ids)` | `GET /v3/settlements` |
| `oa_grade(selections)` | `POST /v3/settlements/grade` |
| `oa_outrights(sport, league, status)` | `GET /v3/outrights` |
| `oa_outright_odds(outright_id, bookmakers)` | `GET /v3/outrights/odds` |
| `oa_outright_results(outright_id)` | `GET /v3/outrights/results` |
| `oa_mappings(bookmaker, ...)` | `GET /v3/mappings` |

Errors keep the API's own message (for example the Outrights add-on notice or the list of markets with history on a movements 404) and add a hint for authentication, plan access and rate limits.

## Links

- Website: https://odds-api.io
- Documentation: https://docs.odds-api.io
- Bug reports: https://github.com/odds-api-io/odds-api-r/issues

## License

MIT

## Citation

If you use this package in research or a publication, please cite it. Citation metadata is in [CITATION.cff](https://github.com/odds-api-io/odds-api-r/blob/main/CITATION.cff) (GitHub renders a "Cite this repository" button from it).

```
Outlier AS. oddsapiio: Official R client for Odds-API.io (version 0.2.0). https://github.com/odds-api-io/odds-api-r
```
