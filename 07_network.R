# ============================================================
# 07_network.R  (Part 1: build network, statistics, centrality)
# RQ4: Which actors occupy the most central positions in the recent
# horror co-appearance network, and is it driven by recurring actors?
# Methods: Module 7 (graphs, degree, density, components, centrality,
# random graphs), Module 7 lab (igraph)
# ============================================================

library(igraph)
library(dplyr)

cast = readRDS("data/clean/cast_clean.rds")
clusters = readRDS("data/clean/horror_clusters.rds")

# ------------------------------------------------------------
# 1. Edge list: every pair of actors who appear in the same film
# ------------------------------------------------------------
films_cast = split(cast$actor_id, cast$film_id)
pairs = lapply(films_cast, function(a) {
  a = sort(unique(a))
  if (length(a) < 2) return(NULL)
  t(combn(a, 2))
})
el = as.data.frame(do.call(rbind, pairs))
names(el) = c("from", "to")
el$from = as.character(el$from)
el$to = as.character(el$to)

edges = el %>% count(from, to, name = "weight")
print(nrow(el))
print(nrow(edges))
print(table(edges$weight))

# ------------------------------------------------------------
# 2. Node table: films per actor, number of themes, main theme
# ------------------------------------------------------------
actor_films = cast %>%
  left_join(clusters[, c("id", "theme")], by = c("film_id" = "id"))

main_theme = function(x) names(sort(table(x), decreasing = TRUE))[1]

nodes = actor_films %>%
  group_by(actor_id) %>%
  summarise(actor_name = first(actor_name),
            n_films = n_distinct(film_id),
            n_themes = n_distinct(theme),
            theme = main_theme(theme),
            .groups = "drop")
nodes$actor_id = as.character(nodes$actor_id)

# ------------------------------------------------------------
# 3. Build the undirected, weighted graph
# ------------------------------------------------------------
g = graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

cat("\nNodes:", vcount(g), " Edges:", ecount(g), "\n")
cat("Density:", signif(edge_density(g), 4), "\n")

# ------------------------------------------------------------
# 4. Recurring actors
# ------------------------------------------------------------
print(table(pmin(nodes$n_films, 5)))
cat("Share of actors in 2+ horror films:", round(mean(nodes$n_films >= 2), 3), "\n")

# ------------------------------------------------------------
# 5. Components
# ------------------------------------------------------------
comp = components(g)
cat("Number of components:", comp$no, "\n")
cat("Largest component:", max(comp$csize), "actors (",
    round(max(comp$csize) / vcount(g), 3), "of all actors )\n")
print(head(sort(comp$csize, decreasing = TRUE), 10))

lcc = induced_subgraph(g, which(comp$membership == which.max(comp$csize)))
cat("Films represented in largest component:",
    length(unique(cast$film_id[as.character(cast$actor_id) %in% V(lcc)$name])), "\n")
cat("LCC diameter:", diameter(lcc, weights = NA), "\n")
cat("LCC mean distance:", round(mean_distance(lcc, weights = NA), 2), "\n")

# ------------------------------------------------------------
# 6. Degree distribution
# ------------------------------------------------------------
deg = degree(g)
print(summary(deg))

# ------------------------------------------------------------
# 7. Centrality: degree and betweenness (unweighted)
# ------------------------------------------------------------
btw = betweenness(g, weights = NA)

cent = data.frame(
  actor = V(g)$actor_name,
  n_films = V(g)$n_films,
  n_themes = V(g)$n_themes,
  theme = V(g)$theme,
  degree = deg,
  betweenness = round(btw)
)

cat("\nTop 10 by degree:\n")
print(head(cent[order(-cent$degree), ], 10), row.names = FALSE)
cat("\nTop 10 by betweenness:\n")
print(head(cent[order(-cent$betweenness), ], 10), row.names = FALSE)

# ------------------------------------------------------------
# 8. Link to RQ3: do bridging actors span more themes?
# ------------------------------------------------------------
recurring = cent$n_films >= 2
top20 = order(-cent$betweenness)[1:20]
cat("\nMean themes, top 20 betweenness:", round(mean(cent$n_themes[top20]), 2), "\n")
cat("Mean themes, all recurring actors:", round(mean(cent$n_themes[recurring]), 2), "\n")
cat("Share of recurring actors in 2+ themes:",
    round(mean(cent$n_themes[recurring] >= 2), 3), "\n")

saveRDS(g, "data/clean/actor_network.rds")
saveRDS(cent, "data/clean/actor_centrality.rds")

# ============================================================
# Part 2: robustness check, theme check, figures
# ============================================================
library(igraph)
library(RColorBrewer)

g = readRDS("data/clean/actor_network.rds")
clusters = readRDS("data/clean/horror_clusters.rds")
V(g)$btw = betweenness(g, weights = NA)
V(g)$deg = degree(g)

themes = c("Mixed / mystery", "Woman in peril", "Slasher & serial killer",
           "School & teen", "Family & haunted home", "Vengeful spirits",
           "Group survival", "Small-town terror")
pal = brewer.pal(8, "Dark2")
pal[1] = "grey75"
names(pal) = themes
V(g)$color = pal[V(g)$theme]

# ------------------------------------------------------------
# 1. Robustness (Module 7: social networks are robust to random
#    removal but fragile when hubs are removed)
# ------------------------------------------------------------
lcc_share = function(gr) round(max(components(gr)$csize) / vcount(g), 3)
top_ids = V(g)$name[order(-V(g)$btw)[1:20]]
set.seed(3020)
rand_ids = sample(V(g)$name[V(g)$n_films >= 2], 20)
cat("LCC share, original:", lcc_share(g), "\n")
cat("LCC share, remove top 20 bridges:", lcc_share(delete_vertices(g, top_ids)), "\n")
cat("LCC share, remove 20 random recurring actors:", lcc_share(delete_vertices(g, rand_ids)), "\n")

# ------------------------------------------------------------
# 2. Theme check: do actors with 2 films cross themes more or less
#    than expected by chance?
# ------------------------------------------------------------
two = V(g)$n_films == 2
observed = mean(V(g)$n_themes[two] == 2)
p_theme = prop.table(table(clusters$theme))
expected = 1 - sum(p_theme^2)
cat("Actors with exactly 2 films:", sum(two), "\n")
cat("Observed share with 2 different themes:", round(observed, 3), "\n")
cat("Expected share if films were random:", round(expected, 3), "\n")

# ------------------------------------------------------------
# 3. Figure 8: degree distribution vs Erdos-Renyi and Barabasi-Albert
#    (same number of nodes; ER with the same density; BA with m = 7,
#    which gives a similar mean degree of about 14)
# ------------------------------------------------------------
set.seed(3020)
g_er = sample_gnp(vcount(g), edge_density(g))
g_ba = sample_pa(vcount(g), m = 7, directed = FALSE)
plot_dd = function(gr, title) {
  dd = degree_distribution(gr)
  plot(0:(length(dd) - 1), dd, type = "h", xlim = c(0, 60),
       xlab = "Degree", ylab = "Proportion of actors", main = title)
}
png("figures/fig8_degree_distribution.png", width = 2400, height = 900, res = 200)
par(mfrow = c(1, 3))
plot_dd(g, "Horror co-appearance network")
plot_dd(g_er, "Erdos-Renyi (same density)")
plot_dd(g_ba, "Barabasi-Albert (m = 7)")
dev.off()

# ------------------------------------------------------------
# 4. Figure 9: full network (all 12,773 actors)
# ------------------------------------------------------------
set.seed(3020)
lay_full = layout_with_drl(g)
png("figures/fig9_full_network.png", width = 2400, height = 2400, res = 250)
plot(g, layout = lay_full, vertex.label = NA, vertex.frame.color = NA,
     vertex.size = 0.3 + V(g)$deg / 40, vertex.color = V(g)$color,
     edge.color = adjustcolor("grey50", alpha.f = 0.08), edge.width = 0.3,
     main = "Horror co-appearance network, 2022-2026 (12,773 actors)")
legend("bottomleft", legend = themes, col = pal, pch = 16, cex = 0.6,
       title = "Actor's main theme", bg = "white")
dev.off()

# ------------------------------------------------------------
# 5. Figure 10: subgraph of recurring actors (the network's backbone)
#    Keep actors in 2+ horror films, then the largest connected part.
#    Centrality values are from the full network (as in the Module 7 lab).
# ------------------------------------------------------------
g_rec = induced_subgraph(g, V(g)$n_films >= 2)
g_rec = delete_vertices(g_rec, degree(g_rec) == 0)
cr = components(g_rec)
g_back = induced_subgraph(g_rec, cr$membership == which.max(cr$csize))
cat("Backbone subgraph:", vcount(g_back), "actors,", ecount(g_back), "edges\n")

top_lab = rank(-V(g_back)$btw) <= 15
set.seed(3020)
lay_back = layout_with_fr(g_back)
png("figures/fig10_backbone_subgraph.png", width = 2400, height = 2400, res = 250)
plot(g_back, layout = lay_back,
     vertex.size = 1.5 + 8 * sqrt(V(g_back)$btw / max(V(g_back)$btw)),
     vertex.color = V(g_back)$color, vertex.frame.color = NA,
     vertex.label = ifelse(top_lab, V(g_back)$actor_name, NA),
     vertex.label.cex = 0.6, vertex.label.color = "black",
     edge.width = E(g_back)$weight * 0.6,
     edge.color = adjustcolor("grey40", alpha.f = 0.3),
     main = "Recurring horror actors (2+ films): largest connected group")
legend("bottomleft", legend = themes, col = pal, pch = 16, cex = 0.6,
       title = "Actor's main theme", bg = "white")
dev.off()

# ------------------------------------------------------------
# 6. Figure 11: egocentric graph of the top bridge (degree 1.5, Module 7)
# ------------------------------------------------------------
ego_node = V(g)[which.max(V(g)$btw)]
g_ego = make_ego_graph(g, order = 1, nodes = ego_node)[[1]]
cat("Ego network of", ego_node$actor_name, ":", vcount(g_ego), "actors\n")
set.seed(3020)
png("figures/fig11_ego_network.png", width = 2000, height = 2000, res = 250)
plot(g_ego, layout = layout_with_fr(g_ego),
     vertex.size = ifelse(V(g_ego)$name == ego_node$name, 8, 3),
     vertex.color = V(g_ego)$color, vertex.frame.color = NA,
     vertex.label = ifelse(V(g_ego)$n_films >= 2, V(g_ego)$actor_name, NA),
     vertex.label.cex = 0.55, vertex.label.color = "black",
     edge.color = adjustcolor("grey40", alpha.f = 0.25),
     main = paste("Ego network of", ego_node$actor_name))
dev.off()

# ------------------------------------------------------------
# 7. Export centrality table for the report
# ------------------------------------------------------------
cent = readRDS("data/clean/actor_centrality.rds")
top_tab = head(cent[order(-cent$betweenness), ], 15)
write.csv(top_tab, "data/export/top_actors_betweenness.csv", row.names = FALSE)