# Inside the Current Horror Boom

COMP3020 Social Web Analytics, Group 8 project.
Topic: horror films released between January 2022 and June 2026, using TMDb data.

This product uses the TMDB API but is not endorsed or certified by TMDB.

## Research questions
- Context: has horror's share of rated films risen?
- RQ1 (text analysis): how does the language of horror overviews differ from Action, Romance and Animation overviews?
- RQ2 (hypothesis testing): is genre group associated with rating band?
- RQ3 (clustering): which narrative themes appear within horror films?
- RQ4 (network analysis): which actors are central in the horror co-appearance network?

## Files
Scripts, run in this order from the project root:
- 01_collect.R: films (horror and non-horror), yearly counts and genre map from the TMDb API
- 02_cast.R: top 15 billed cast for each horror film
- 03_clean.R: cleaning, saves films_clean.rds and cast_clean.rds
- 04_text.R: RQ1, TF-IDF, word cloud
- 05_hypothesis.R: RQ2, chi-squared test on rating bands
- 06_cluster.R: RQ3, TF-IDF, cosine distance, MDS, k-means
- 07_network.R: RQ4, co-appearance network and centrality
- 08_export.R: writes cleaned data as CSV

Report:
- horror_boom_report_final.Rmd: report source. Knit to PDF with xelatex.

Data and figures:
- data/raw: API output as RDS, collected on 5 October 2026
- data/clean: cleaned data and intermediate objects as RDS
- data/export: cleaned data and results as CSV
- figures: PNG figures produced by scripts 04 to 07

## Reproduce
1. Install R packages: httr2, dplyr, tm, SnowballC, wordcloud, RColorBrewer, igraph, slam.
2. Scripts 01 and 02 need a TMDb read access token. Store it as TMDB_TOKEN in a local .Renviron file. Do not commit this file. TMDb data changes over time, so a new collection gives different numbers.
3. To reproduce the analysis without the API, skip 01 and 02. The raw data is in data/raw. Run 03 to 08.
4. Open the project in the folder root and knit horror_boom_report_final.Rmd. The report reads data/raw/*.rds and does not call the API.
