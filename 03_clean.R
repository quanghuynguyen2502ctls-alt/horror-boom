library(dplyr)

horror_raw = readRDS("data/raw/horror_raw.rds")
nonhorror_raw = readRDS("data/raw/nonhorror_raw.rds")
cast_raw = readRDS("data/raw/cast_raw.rds")

horror_raw$group = "Horror"
nonhorror_raw$group = "Non-horror"
films = rbind(horror_raw, nonhorror_raw)

has_genre = function(ids, g) grepl(paste0("(^|,)", g, "(,|$)"), ids)
films$is_horror = has_genre(films$genre_ids, 27)
films$is_action = has_genre(films$genre_ids, 28)
films$is_romance = has_genre(films$genre_ids, 10749)
films$is_animation = has_genre(films$genre_ids, 16)

print(sum(duplicated(films$id)))
print(colSums(is.na(films)))
print(all(films$is_horror[films$group == "Horror"]))
print(any(films$is_horror[films$group == "Non-horror"]))

films$overview = trimws(films$overview)
films = films[films$overview != "", ]
films$n_words = lengths(strsplit(films$overview, "\\s+"))
films$release_year = as.integer(substr(films$release_date, 1, 4))

print(table(films$group))
print(colSums(films[films$group == "Non-horror", c("is_action", "is_romance", "is_animation")]))
print(table(films$release_year, films$group))
print(summary(films$n_words))
print(head(films[order(films$n_words), c("title", "group", "n_words", "overview")], 8))

cast_clean = cast_raw[cast_raw$film_id %in% films$id[films$group == "Horror"], ]
print(nrow(cast_clean))

saveRDS(films, "data/clean/films_clean.rds")
saveRDS(cast_clean, "data/clean/cast_clean.rds")