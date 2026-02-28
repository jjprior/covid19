# covid19-r — Claude Project Notes

## What This Is
Shiny R app for COVID-19 historical data analysis (`app.R`). Data sources (NYT, OWID) are frozen archives. The app runs entirely from a local cache — no network or AWS access needed.

## Data
- `alldata.RData` (25MB) — loaded at startup, contains:
  - `amerData`: US state data (~55k rows)
  - `worldData`: Country data (~331k rows)
  - `allData`: Combined + aggregates (~392k rows)
  - `USStateData`, `deepBlueStateData`, `deepRedStateData`: political rollups
  - `world`: raw world object (unused in main flow)
- Raw data extends to 2024-08-14, but **hard-capped at 2023-04-30** at startup (data quality degrades after that)
- `worldpops.Rda` — world population lookup table

## Key Architecture
- `endDate` — derived from `max(allData$rdate)` at startup; drives all slider defaults, forecast windows, and growth models
- `mday` — regression covariate = `difftime(rdate, endDate, units="days")`; must be consistent across all data frames; recomputed via `fix_mday()` after load
- `covid_calc()` — adds all computed columns (cfr, fracpos, mday, growth projections, etc.); runs per-state linear models; expensive
- `get_hot_spots()` — called at UI construction time to set default selections; reads pre-computed `projectedCaseGrowth` column, fast

## Startup Sequence (bottom of app.R)
```r
load("alldata.RData")
endDate = as.Date(max(allData$rdate, na.rm = TRUE))
fix_mday = function(x) { x$mday = as.numeric(difftime(x$rdate, endDate, units = "days")); x }
amerData  = fix_mday(amerData)
worldData = fix_mday(worldData)
allData   = fix_mday(allData)
# optional aggregates checked with exists()
shinyApp(ui = ui, server = server, enableBookmarking = "url")
```

## Known Bugs (as of Feb 2026 review)

### Active bugs to fix:
1. **`plot_xy` line 668** — `!is.na(sdata$y)&!is.na(sdata$y)` should be `!is.na(sdata$x)&!is.na(sdata$y)`
2. **`plot_trend` line 759** — `forecast$pop` doesn't exist in the forecast data frame returned by `calc_forecast`; the population cap guard silently never fires
3. **`rawdata()` Custom scope** — missing `"All States" → electoralAll` mapping (present in `generate_plot()` but not in `rawdata()`)
4. **`customData` undefined** — line 1243 defines `ccFun` referencing `customData` which is never created; dead code but latent error

### Minor/cosmetic:
5. **`region_aggregate` line 270** — `Regionpeople_vaccinated` aggregate duplicated (identical line 268 and 270)
6. **`output$Plot0`** passes `allData` as data arg to `generate_plot()` but it's immediately overridden inside the function — harmless but confusing
7. **Estate/scope logic duplicated** between `rawdata()` and `generate_plot()` — can diverge

### Dead/latent (not called at startup):
- `get_world_data()` and `get_amer_data()` use `rvac`/`data`/`worlddata` outside the `try({})` blocks that create them — would fail if called with `refreshData=FALSE`

## Fixes Applied (session 1, Feb 2026)
1. `forceRefresh = FALSE` — was TRUE, caused failed web fetches on every start
2. `endDate` derived from `max(allData$rdate)` after load, not hardcoded
3. `mday` recomputed across all data frames after `endDate` is set (`fix_mday`)
4. All AWS `put_object()`/`save_object()` calls wrapped in `try()`
5. Local cache checked first; S3 only as fallback
6. `save()` includes USStateData, deepBlueStateData, deepRedStateData
7. `output$Plot0` fixed — was referencing unbound variables
8. UI sliders: `value` and `max` use derived `endDate` instead of hardcoded dates
9. `plot_xy()` bug fixed: `feature` → `yfeature`
10. Refresh buttons renamed; title updated to show historical date range

## Security Note
`awskey.R` contains hardcoded AWS credentials — not committed to git, but credentials should be rotated and moved to environment variables.
