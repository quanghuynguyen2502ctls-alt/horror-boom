library(httr2)

token = Sys.getenv("TMDB_TOKEN")
horror_raw = readRDS("data/raw/horror_raw.rds")

get_cast = function(movie_id, n_cast = 15) {
  res = request(paste0("https://api.themoviedb.org/3/movie/", movie_id, "/credits")) |>
    req_auth_bearer_token(token) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json()
  Sys.sleep(0.1)
  cast = res$cast
  if (length(cast) == 0) return(NULL)
  orders = sapply(cast, function(a) a$order)
  cast = cast[order(orders)]
  cast = cast[1:min(n_cast, length(cast))]
  do.call(rbind, lapply(cast, function(a) data.frame(
    film_id = movie_id,
    actor_id = a$id,
    actor_name = a$name,
    cast_order = a$order,
    character = ifelse(is.null(a$character), "", a$character)
  )))
}

ids = horror_raw$id
cast_list = vector("list", length(ids))
failed = c()

for (i in seq_along(ids)) {
  cast_list[[i]] = tryCatch(
    get_cast(ids[i]),
    error = function(e) {
      failed <<- c(failed, ids[i])
      NULL
    }
  )
  if (i %% 100 == 0) cat("Done", i, "of", length(ids), "\n")
}

cast_raw = do.call(rbind, cast_list)
saveRDS(cast_raw, "data/raw/cast_raw.rds")

print(nrow(cast_raw))
print(length(unique(cast_raw$film_id)))
print(length(unique(cast_raw$actor_id)))
print(failed)
print(summary(as.vector(table(cast_raw$film_id))))
films_per_actor = table(cast_raw$actor_id)
print(mean(films_per_actor >= 2))
print(head(sort(table(cast_raw$actor_name), decreasing = TRUE), 15))