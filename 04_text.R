# ============================================================
# 04_text.R
# RQ1: How does the language of recent horror film overviews
# differ from that of non-horror films (Action, Romance, Animation)?
# Methods: preprocessing (Module 4 Part 1), TF-IDF (Module 4 Part 2),
# word cloud (Module 5)
# ============================================================

library(tm)
library(SnowballC)
library(wordcloud)

dir.create("figures", showWarnings = FALSE)

films = readRDS("data/clean/films_clean.rds")

# ------------------------------------------------------------
# 1. Summary statistics: overview length by group
# ------------------------------------------------------------
print(table(films$group))
print(tapply(films$n_words, films$group, summary))

# ------------------------------------------------------------
# 2. Preprocessing (same order as the Module 4 lab)
# ------------------------------------------------------------
# Custom stopwords: generic plot-summary words (already stemmed)
# that ranked in the top 40 for BOTH horror and non-horror,
# so they carry no information about genre.
custom_stop = c("find", "must", "one", "two", "begin", "forc", "take", "discov",
                "becom", "turn", "will", "get", "soon", "new", "make", "can",
                "back", "time", "year")

corpus = Corpus(VectorSource(films$overview))
corpus = tm_map(corpus, function(x) iconv(x, to = "ASCII", sub = " "))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, removeWords, custom_stop)
corpus = tm_map(corpus, stripWhitespace)

# ------------------------------------------------------------
# 3. Document-term matrix (rows = films, columns = terms)
# ------------------------------------------------------------
dtm = DocumentTermMatrix(corpus)
print(dim(dtm))
print(sum(slam::row_sums(dtm) == 0))
saveRDS(dtm, "data/clean/dtm_all.rds")

# ------------------------------------------------------------
# 4. TF-IDF with the lecture formula: TF = log(f + 1), IDF = log(N / df)
#    Computed on the sparse matrix to avoid a very large dense matrix.
# ------------------------------------------------------------
N = nrow(dtm)
df = tabulate(dtm$j, nbins = ncol(dtm))
idf = log(N / df)
tfidf = dtm
tfidf$v = log(dtm$v + 1) * idf[dtm$j]
saveRDS(tfidf, "data/clean/tfidf_all.rds")

# ------------------------------------------------------------
# 5. Comparison groups
# ------------------------------------------------------------
groups = list(
  Horror = films$group == "Horror",
  Action = films$group == "Non-horror" & films$is_action,
  Romance = films$group == "Non-horror" & films$is_romance,
  Animation = films$group == "Non-horror" & films$is_animation
)
print(sapply(groups, sum))

# Mean TF-IDF weight of each term within a group (as in the Module 6 lab)
group_mean = function(rows) slam::col_sums(tfidf[rows, ]) / sum(rows)
top_terms = lapply(groups, function(r) head(sort(group_mean(r), decreasing = TRUE), 10))
print(top_terms)

# ------------------------------------------------------------
# 6. Frequent words in horror overviews (raw frequency)
# ------------------------------------------------------------
freq_h = sort(slam::col_sums(dtm[groups$Horror, ]), decreasing = TRUE)
print(head(freq_h, 30))

# ------------------------------------------------------------
# 7. Figure 1: word cloud of horror overviews
# ------------------------------------------------------------
png("figures/fig1_horror_wordcloud.png", width = 1600, height = 1600, res = 200)
set.seed(3020)
wordcloud(names(freq_h), freq_h, max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Dark2"), scale = c(3.5, 0.6))
title("Most frequent words in horror overviews (2022-2026)")
dev.off()

# ------------------------------------------------------------
# 8. Figure 2: top TF-IDF terms by genre
# ------------------------------------------------------------
png("figures/fig2_tfidf_by_genre.png", width = 2000, height = 1600, res = 200)
par(mfrow = c(2, 2), mar = c(4, 7, 3, 1))
for (g in names(top_terms)) {
  barplot(rev(top_terms[[g]]), horiz = TRUE, las = 1,
          main = g, xlab = "Mean TF-IDF weight", col = "grey40")
}
dev.off()

cat("Figures saved in the 'figures' folder.\n")