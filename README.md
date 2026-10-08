# Inside the Current Horror Boom (COMP3020 Group 8)

Analysis of horror films released January 2022 to June 2026, using data from the TMDb API.
This product uses the TMDB API but is not endorsed or certified by TMDB.

## Files
- `horror_boom_report.Rmd`: full analysis (data collection code, text analysis, hypothesis tests, clustering, network analysis)
- `horror_boom_report.pdf`: rendered report
- `data/raw/`: data collected from TMDb (films, cast, yearly counts)

## Reproduce the report
1. Install R packages:
   `install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer", "dplyr", "igraph", "slam", "httr2", "jsonlite", "tinytex"))`
2. Open `horror_boom_report.Rmd` in RStudio and click **Knit**. The report reads the saved data in `data/raw/`, so no API access is needed.

## Re-collect the data (optional)
1. Get a free API Read Access Token from https://developer.themoviedb.org/
2. Add `TMDB_TOKEN=your_token` to a `.Renviron` file (never commit this file).
3. Run the code in the `collection` chunk. Note: TMDb data changes over time, so results will differ slightly from the report.
