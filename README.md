# Inside the Current Horror Boom

COMP3020 Social Web Analytics, Group 8 (Western Sydney University, Spring 2026)

Themes, audience ratings and actors in horror films released between January 2022 and June 2026, using data from the TMDb API.

## Group members

| Name | Student ID |
|:--|:--|
| Minh Duc Nguyen | 22153839 |
| Quang Huy Nguyen | 22097631 |
| Phuong Thao Phi | 22192468 |

## Research questions

- **RQ1 (text analysis):** How does the language of recent horror film overviews differ from that of non-horror films (Action, Romance, Animation)?
- **RQ2 (hypothesis testing):** Is genre group (horror vs non-horror) associated with audience rating band?
- **RQ3 (clustering):** What distinct narrative themes emerge within recent horror films?
- **RQ4 (network analysis):** Which actors occupy the most central positions in the recent horror co-appearance network, and is the network driven by a small group of recurring actors?

## Repository structure

```
horror-boom/
  horror_boom_report_final.Rmd   report source (knits to PDF)
  01_collect.R                   collects films and yearly counts from TMDb
  02_cast.R                      collects top 15 billed cast for each horror film
  data/
    raw/
      horror_raw.rds             1,176 horror films (genre 27)
      nonhorror_raw.rds          2,472 Action, Romance and Animation films without Horror
      cast_raw.rds               14,558 film-actor records for horror films
      yearly_counts.rds          horror and total rated films per year, 2015-2026
      genre_map.rds              TMDb genre IDs and names
    clean/
      films_clean.rds            3,639 films after removing empty overviews
      cast_clean.rds             14,538 cast records for the cleaned horror films
    export/
      films_clean.csv            CSV copy of films_clean
      cast_clean.csv             CSV copy of cast_clean
      yearly_counts.csv          CSV copy of yearly_counts
      genre_map.csv              CSV copy of genre_map
```

## Data collection

- **Source:** The Movie Database (TMDb) API v3, `discover/movie` and `movie/{id}/credits` endpoints
- **Collected on:** 5 October 2026
- **Release window:** 1 January 2022 to 30 June 2026
- **Filters:** at least 20 votes, `language = en-US`, `include_adult = false`
- **Horror group:** genre ID 27
- **Comparison group:** Action (28), Romance (10749) or Animation (16), excluding any film tagged Horror

## How to reproduce

1. Install R packages:
```r
   install.packages(c("httr2", "tm", "SnowballC", "wordcloud",
                      "RColorBrewer", "dplyr", "igraph", "slam",
                      "knitr", "rmarkdown"))
```
2. **Report only (recommended):** open `horror_boom_report_final.Rmd` in RStudio and click **Knit**. The report reads the saved files in `data/raw/`, so no API access is needed.
3. **Re-collecting the data (optional):** get a free TMDb API Read Access Token, add the line `TMDB_TOKEN=your_token_here` to a `.Renviron` file in the project folder, restart R, then run `01_collect.R` followed by `02_cast.R`. TMDb data changes over time, so a new collection will not exactly match the saved data.

## Notes

- `.Renviron` is not included because it contains a private API token.
- This product uses the TMDB API but is not endorsed or certified by TMDB.
