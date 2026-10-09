# ============================================================
# 05_hypothesis.R
# RQ2: Is genre group (horror vs non-horror) associated with
# audience rating band?
# Method: chi-squared test for independence with simulated
# p-value (Module 3)
# ============================================================

films = readRDS("data/clean/films_clean.rds")
dir.create("figures", showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Descriptive statistics of ratings and visibility
# ------------------------------------------------------------
print(tapply(films$vote_average, films$group, summary))
print(tapply(films$vote_count, films$group, median))
print(tapply(films$popularity, films$group, median))

# ------------------------------------------------------------
# 2. Rating bands: tertiles of vote_average across all films
# ------------------------------------------------------------
cuts = quantile(films$vote_average, probs = c(0, 1/3, 2/3, 1))
print(cuts)
films$rating_band = cut(films$vote_average, breaks = cuts,
                        labels = c("Low", "Medium", "High"),
                        include.lowest = TRUE)
print(table(films$rating_band))

# ------------------------------------------------------------
# 3. Contingency table
# ------------------------------------------------------------
tab = table(films$group, films$rating_band)
print(tab)
print(round(prop.table(tab, margin = 1), 3))

# ------------------------------------------------------------
# 4. Chi-squared test with simulated p-value
# H0: genre group and rating band are independent
# H1: genre group and rating band are not independent
# ------------------------------------------------------------
set.seed(3020)
ct = chisq.test(tab, simulate.p.value = TRUE)
print(ct)
print(round(ct$expected, 1))
print(round(ct$residuals, 2))

saveRDS(films, "data/clean/films_with_bands.rds")

# ------------------------------------------------------------
# 5. Figure 3: rating band proportions by group
# ------------------------------------------------------------
png("figures/fig3_rating_band_by_group.png", width = 1600, height = 1200, res = 200)
barplot(prop.table(tab, margin = 1), beside = TRUE,
        col = c("firebrick", "grey60"), ylim = c(0, 0.6),
        legend.text = TRUE, args.legend = list(x = "topright"),
        xlab = "Rating band (tertiles of TMDb vote average)",
        ylab = "Proportion of films in group",
        main = "Rating bands: horror vs non-horror (2022-2026)")
dev.off()

# ------------------------------------------------------------
# 6. Figure 4: visibility (vote count) by group
# ------------------------------------------------------------
png("figures/fig4_vote_count_by_group.png", width = 1400, height = 1200, res = 200)
boxplot(vote_count ~ group, data = films, log = "y",
        col = c("firebrick", "grey60"),
        ylab = "Vote count (log scale)", xlab = "",
        main = "Audience engagement: number of votes")
dev.off()