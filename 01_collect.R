library(httr2)

token <- Sys.getenv("TMDB_TOKEN")
nchar(token)

req <- request("https://api.themoviedb.org/3/discover/movie") |>
  req_auth_bearer_token(token) |>
  req_url_query(
    with_genres = 27,
    `primary_release_date.gte` = "2022-01-01",
    `primary_release_date.lte` = "2026-06-30",
    `vote_count.gte` = 20,
    include_adult = "false",
    language = "en-US",
    page = 1
  )

resp <- req_perform(req)
resp_status(resp)

res <- resp_body_json(resp)
res$total_results
res$total_pages

res$results[[1]]$title
res$results[[1]]$overview
names(res$results[[1]])

count_films = function(start, end, genre = NULL) {
  req = request("https://api.themoviedb.org/3/discover/movie") |>
    req_auth_bearer_token(token) |>
    req_url_query(
      `primary_release_date.gte` = start,
      `primary_release_date.lte` = end,
      `vote_count.gte` = 20,
      include_adult = "false",
      page = 1
    )
  if (!is.null(genre)) {
    req = req |> req_url_query(with_genres = genre)
  }
  res = req |> req_perform() |> resp_body_json()
  Sys.sleep(0.1)
  return(res$total_results)
}

years = 2015:2026
horror = rep(0, length(years))
total = rep(0, length(years))

for (i in seq_along(years)) {
  start = paste0(years[i], "-01-01")
  end = ifelse(years[i] == 2026, "2026-06-30", paste0(years[i], "-12-31"))
  horror[i] = count_films(start, end, genre = 27)
  total[i] = count_films(start, end)
}

yearly = data.frame(year = years, horror = horror, total = total,
                    share = round(horror / total, 3))
yearly

saveRDS(yearly, "data/raw/yearly_counts.rds")

pre = yearly$year %in% 2015:2019
post = yearly$year %in% 2022:2026

period_tab = matrix(c(
  sum(yearly$horror[pre]),  sum(yearly$total[pre]) - sum(yearly$horror[pre]),
  sum(yearly$horror[post]), sum(yearly$total[post]) - sum(yearly$horror[post])
), nrow = 2, byrow = TRUE)
rownames(period_tab) = c("2015-2019", "2022-2026")
colnames(period_tab) = c("Horror", "Non-horror")
period_tab

ct_period = chisq.test(period_tab, simulate.p.value = TRUE)
ct_period
ct_period$expected

library(dplyr)

get_page = function(page, with_genres, without_genres = NULL) {
  req = request("https://api.themoviedb.org/3/discover/movie") |>
    req_auth_bearer_token(token) |>
    req_url_query(
      with_genres = with_genres,
      `primary_release_date.gte` = "2022-01-01",
      `primary_release_date.lte` = "2026-06-30",
      `vote_count.gte` = 20,
      include_adult = "false",
      language = "en-US",
      sort_by = "primary_release_date.asc",
      page = page
    )
  if (!is.null(without_genres)) {
    req = req |> req_url_query(without_genres = without_genres)
  }
  res = req |> req_perform() |> resp_body_json()
  Sys.sleep(0.1)
  films = lapply(res$results, function(m) data.frame(
    id = m$id,
    title = m$title,
    overview = m$overview,
    genre_ids = paste(unlist(m$genre_ids), collapse = ","),
    vote_average = m$vote_average,
    vote_count = m$vote_count,
    popularity = m$popularity,
    release_date = m$release_date,
    original_language = m$original_language
  ))
  list(films = do.call(rbind, films), total_pages = res$total_pages)
}

get_all = function(with_genres, without_genres = NULL) {
  first = get_page(1, with_genres, without_genres)
  pages = list(first$films)
  if (first$total_pages > 1) {
    for (p in 2:first$total_pages) {
      pages[[p]] = get_page(p, with_genres, without_genres)$films
    }
  }
  do.call(rbind, pages)
}

horror_raw = get_all(with_genres = "27")
nonhorror_raw = get_all(with_genres = "28|10749|16", without_genres = "27")

horror_raw = distinct(horror_raw, id, .keep_all = TRUE)
nonhorror_raw = distinct(nonhorror_raw, id, .keep_all = TRUE)

genres = request("https://api.themoviedb.org/3/genre/movie/list") |>
  req_auth_bearer_token(token) |>
  req_url_query(language = "en") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE)
genre_map = genres$genres

saveRDS(horror_raw, "data/raw/horror_raw.rds")
saveRDS(nonhorror_raw, "data/raw/nonhorror_raw.rds")
saveRDS(genre_map, "data/raw/genre_map.rds")

nrow(horror_raw)
nrow(nonhorror_raw)
sum(horror_raw$overview == "")
sum(nonhorror_raw$overview == "")
head(horror_raw[, c("title", "release_date", "vote_average", "genre_ids")])
genre_map

