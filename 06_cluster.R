# ============================================================
# 06_cluster.R  (Part 1: representation, distance, choosing k)
# RQ3: What distinct narrative themes emerge within recent horror films?
# Method follows the Module 6 lab: TF-IDF -> cosine distance ->
# MDS projection -> k-means, k chosen by the elbow method
# ============================================================

library(tm)

films = readRDS("data/clean/films_with_bands.rds")
dtm = readRDS("data/clean/dtm_all.rds")
dir.create("figures", showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Horror documents only
# ------------------------------------------------------------
h = films$group == "Horror"
dtm_h = dtm[h, ]
films_h = films[h, ]

# ------------------------------------------------------------
# 2. Keep terms that appear in at least min_df horror overviews
#    (removes one-off names that cannot define a shared theme)
# ------------------------------------------------------------
min_df = 5
df_h = tabulate(dtm_h$j, nbins = ncol(dtm_h))
dtm_h = dtm_h[, df_h >= min_df]
print(dim(dtm_h))

M = as.matrix(dtm_h)
empty = rowSums(M) == 0
print(sum(empty))
M = M[!empty, ]
films_h = films_h[!empty, ]

# ------------------------------------------------------------
# 3. TF-IDF with IDF recomputed on horror films only
#    TF = log(f + 1), IDF = log(N / df)   (Module 4 lab)
# ------------------------------------------------------------
N = nrow(M)
idf = log(N / colSums(M > 0))
X = log(M + 1) %*% diag(idf)
colnames(X) = colnames(M)
print(dim(X))

# ------------------------------------------------------------
# 4. Cosine distance: normalise each document to unit length,
#    then squared Euclidean distance / 2 = 1 - cosine similarity
# ------------------------------------------------------------
norm_X = X / sqrt(rowSums(X^2))
D = dist(norm_X)^2 / 2

# ------------------------------------------------------------
# 5. Project cosine distances into Euclidean space with MDS
#    (k-means only works with Euclidean distance; Module 6 lab)
# ------------------------------------------------------------
mds_hi = cmdscale(D, k = 400)
print(dim(mds_hi))

# ------------------------------------------------------------
# 6. Elbow method: Euclidean on raw TF-IDF vs cosine via MDS
# ------------------------------------------------------------
set.seed(3020)
n = 15
SSW_euc = rep(0, n)
SSW_cos = rep(0, n)
for (a in 1:n) {
  SSW_euc[a] = kmeans(X, a, nstart = 20, iter.max = 50)$tot.withinss
  SSW_cos[a] = kmeans(mds_hi, a, nstart = 20, iter.max = 50)$tot.withinss
  cat("k =", a, "done\n")
}

elbow = data.frame(
  k = 1:n,
  euclid_prop = round(SSW_euc / SSW_euc[1], 3),
  cosine_prop = round(SSW_cos / SSW_cos[1], 3),
  cosine_drop = round(c(NA, -diff(SSW_cos / SSW_cos[1])), 4)
)
print(elbow)

png("figures/fig5_elbow.png", width = 1600, height = 1200, res = 200)
plot(1:n, elbow$euclid_prop, type = "b", pch = 16, col = "grey50",
     ylim = range(c(elbow$euclid_prop, elbow$cosine_prop)),
     xlab = "Number of clusters (k)", ylab = "SSW / SST",
     main = "Elbow plot: Euclidean vs cosine distance")
lines(1:n, elbow$cosine_prop, type = "b", pch = 16, col = "firebrick")
legend("topright", legend = c("Euclidean (raw TF-IDF)", "Cosine (via MDS)"),
       col = c("grey50", "firebrick"), pch = 16, lty = 1)
dev.off()

saveRDS(list(X = X, norm_X = norm_X, D = D, mds_hi = mds_hi,
             films_h = films_h, elbow = elbow),
        "data/clean/cluster_inputs.rds")

# ============================================================
# Part 2: compare candidate k values by size and top terms
# ============================================================
inp = readRDS("data/clean/cluster_inputs.rds")
X = inp$X
mds_hi = inp$mds_hi
films_h = inp$films_h

top_words = function(cl, n_words = 8) {
  sapply(sort(unique(cl)), function(c) {
    w = colMeans(X[cl == c, , drop = FALSE])
    paste(names(sort(w, decreasing = TRUE))[1:n_words], collapse = ", ")
  })
}

candidates = list()
for (k in c(4, 6, 8)) {
  set.seed(3020)
  K = kmeans(mds_hi, k, nstart = 50, iter.max = 100)
  candidates[[as.character(k)]] = K$cluster
  cat("\n========== k =", k, "==========\n")
  print(table(K$cluster))
  tw = top_words(K$cluster)
  for (c in seq_along(tw)) {
    ex = head(films_h$title[K$cluster == c], 3)
    cat("Cluster", c, ":", tw[c], "\n   e.g.", paste(ex, collapse = " | "), "\n")
  }
}

saveRDS(candidates, "data/clean/cluster_candidates.rds")

# ============================================================
# Part 3: final solution, k = 8
# ============================================================
library(RColorBrewer)

inp = readRDS("data/clean/cluster_inputs.rds")
X = inp$X
D = inp$D
films_h = inp$films_h
cl = readRDS("data/clean/cluster_candidates.rds")[["8"]]

themes = c("Mixed / mystery", "Woman in peril", "Slasher & serial killer",
           "School & teen", "Family & haunted home", "Vengeful spirits",
           "Group survival", "Small-town terror")
films_h$cluster = cl
films_h$theme = themes[cl]

top_words = function(cl, n_words = 8) {
  sapply(sort(unique(cl)), function(c) {
    w = colMeans(X[cl == c, , drop = FALSE])
    paste(names(sort(w, decreasing = TRUE))[1:n_words], collapse = ", ")
  })
}

# ------------------------------------------------------------
# 1. Cluster summary: size, top terms, ratings (link to RQ2)
# ------------------------------------------------------------
summary_tab = data.frame(
  cluster = 1:8,
  theme = themes,
  n = as.vector(table(cl)),
  share = round(as.vector(table(cl)) / length(cl), 3),
  median_rating = round(as.vector(tapply(films_h$vote_average, cl, median)), 2),
  median_votes = as.vector(tapply(films_h$vote_count, cl, median)),
  pct_high = round(as.vector(tapply(films_h$rating_band == "High", cl, mean)), 3),
  pct_low = round(as.vector(tapply(films_h$rating_band == "Low", cl, mean)), 3)
)
print(summary_tab)

tw = top_words(cl, 10)
for (c in 1:8) cat(c, themes[c], ":", tw[c], "\n")

# ------------------------------------------------------------
# 2. Best-known example films per cluster (most votes)
# ------------------------------------------------------------
for (c in 1:8) {
  sub = films_h[cl == c, ]
  ex = head(sub$title[order(-sub$vote_count)], 5)
  cat(c, themes[c], ":", paste(ex, collapse = " | "), "\n")
}

# ------------------------------------------------------------
# 3. Theme x rating band (descriptive link to RQ2)
# ------------------------------------------------------------
band_tab = table(films_h$theme, films_h$rating_band)
print(band_tab)
set.seed(3020)
ct3 = chisq.test(band_tab, simulate.p.value = TRUE)
print(ct3)
print(round(ct3$expected, 1))
print(round(ct3$residuals, 2))

# ------------------------------------------------------------
# 4. Figure 6: clusters on a 2D MDS projection (cosine distance)
#    Clusters were computed in 400 dimensions; 2D is for display only.
# ------------------------------------------------------------
mds2 = cmdscale(D, k = 2)
cols = brewer.pal(8, "Dark2")
cols[1] = "grey80"
o = order(cl != 1)
png("figures/fig6_clusters_mds.png", width = 1800, height = 1500, res = 200)
plot(mds2[o, ], col = cols[cl[o]], pch = 16, cex = 0.7,
     xlab = "MDS dimension 1", ylab = "MDS dimension 2",
     main = "Narrative themes in horror overviews (k = 8, cosine distance)")
legend("topright", legend = themes, col = cols, pch = 16, cex = 0.65, bg = "white")
dev.off()

# ------------------------------------------------------------
# 5. Figure 7: top terms of each cluster
# ------------------------------------------------------------
png("figures/fig7_cluster_terms.png", width = 2600, height = 1400, res = 200)
par(mfrow = c(2, 4), mar = c(4, 6, 3, 1))
for (c in 1:8) {
  w = sort(colMeans(X[cl == c, , drop = FALSE]), decreasing = TRUE)[1:8]
  barplot(rev(w), horiz = TRUE, las = 1, main = themes[c],
          col = ifelse(c == 1, "grey60", cols[c]),
          xlab = "Mean TF-IDF", cex.names = 0.85, cex.main = 0.9)
}
dev.off()

# ------------------------------------------------------------
# 6. Save cluster labels for the network analysis (RQ4)
# ------------------------------------------------------------
horror_clusters = films_h[, c("id", "title", "cluster", "theme",
                              "vote_average", "vote_count", "rating_band")]
saveRDS(horror_clusters, "data/clean/horror_clusters.rds")
write.csv(horror_clusters, "data/export/horror_clusters.csv", row.names = FALSE)