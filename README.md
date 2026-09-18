# oddsapiio

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/oddsapiio)](https://CRAN.R-project.org/package=oddsapiio)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/license/mit)
<!-- badges: end -->

Official R client for [Odds-API.io](https://odds-api.io), a real-time sports betting odds API covering 265+ bookmakers across 34 sports and 12,000+ leagues.

- Odds-API.io: https://odds-api.io
- Documentation: https://docs.odds-api.io

The free tier includes 100 requests/hour, no credit card required.

## Installation

Once on CRAN:

```r
install.packages("oddsapiio")
```

Development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("odds-api-io/odds-api-r")
```

## Authentication

Sign up at [odds-api.io](https://odds-api.io) to get a free API key, then set it as an environment variable (for example in `.Renviron`):

```r
Sys.setenv(ODDS_API_KEY = "your-key")
```

Every function also accepts an explicit `api_key` argument.

## Quick start

```r
library(oddsapiio)

# Sports and bookmakers (no API key needed)
oa_sports()
#>         name       slug
#> 1   Football   football
#> 2 Basketball basketball
#> 3     Tennis     tennis
#> ...

# Leagues for a sport
oa_leagues("football")
#>                       name                   slug eventsCount
#> 1 England - Premier League england-premier-league          32
#> 2          Spain - La Liga          spain-la-liga          28
#> ...

# Upcoming events
events <- oa_events("football", league = "england-premier-league")
head(events[, c("id", "home", "away", "date", "status")])
#>       id              home      away                 date  status
#> 1 123456 Manchester United Liverpool 2026-08-15T15:00:00Z pending
#> ...

# Odds for one event, tidy: one row per price line
odds <- oa_odds(events$id[1], bookmakers = c("Bet365", "SingBet"))
subset(odds, market == "ML")
#>   eventId bookmaker market            updatedAt home draw away
#> 1  123456    Bet365     ML 2026-08-14T10:30:00Z 2.10 3.40 3.20

# Value bets and arbitrage opportunities
oa_value_bets("Bet365", include_event_details = TRUE)
oa_arbitrage_bets(c("Bet365", "SingBet"))
```

## Functions

| Function | Endpoint |
| --- | --- |
| `oa_sports()` | `GET /v3/sports` |
| `oa_bookmakers()` | `GET /v3/bookmakers` |
| `oa_leagues(sport)` | `GET /v3/leagues` |
| `oa_events(sport, ...)` | `GET /v3/events` |
| `oa_odds(event_id, bookmakers)` | `GET /v3/odds` |
| `oa_value_bets(bookmaker, ...)` | `GET /v3/value-bets` |
| `oa_arbitrage_bets(bookmakers, ...)` | `GET /v3/arbitrage-bets` |

## Links

- Website: https://odds-api.io
- Documentation: https://docs.odds-api.io
- Bug reports: https://github.com/odds-api-io/odds-api-r/issues

## License

MIT

## Citation

If you use this package in research or a publication, please cite it. Citation metadata is in [CITATION.cff](CITATION.cff) (GitHub renders a "Cite this repository" button from it) and Zenodo metadata is in `.zenodo.json`.

```
Outlier AS. oddsapiio: Official R client for Odds-API.io (version 0.1.1). https://github.com/odds-api-io/odds-api-r
```
