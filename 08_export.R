# 08_export.R: write the cleaned data and yearly counts as CSV for readers without R.
# Run after 03_clean.R. Run from the project root.
films = readRDS("data/clean/films_clean.rds")
cast = readRDS("data/clean/cast_clean.rds")
yearly = readRDS("data/raw/yearly_counts.rds")
genre_map = readRDS("data/raw/genre_map.rds")
dir.create("data/export", recursive = TRUE, showWarnings = FALSE)
write.csv(films, "data/export/films_clean.csv", row.names = FALSE)
write.csv(cast, "data/export/cast_clean.csv", row.names = FALSE)
write.csv(yearly, "data/export/yearly_counts.csv", row.names = FALSE)
write.csv(genre_map, "data/export/genre_map.csv", row.names = FALSE)