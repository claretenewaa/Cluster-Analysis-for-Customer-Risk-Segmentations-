# =============================================================================
# 03_cluster_analysis.R
#
# Core analysis: K-means model selection (elbow + silhouette), a final
# K-means (k=6) fit, a complementary DBSCAN pass, cluster profiling, and
# the figures used in the writeup.
#
# Run after 02_clean_and_preprocess.R.
# =============================================================================

suppressMessages({library(dplyr); library(ggplot2); library(cluster); library(dbscan)})
set.seed(602)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/data", showWarnings = FALSE, recursive = TRUE)

features <- read.csv("data/cluster_features.csv")
X <- as.matrix(features[, -1])
raw <- readRDS("data/bank_data_clean.rds")
raw_ord <- raw[match(features$customer_id, raw$customer_id), ]

cat("Feature matrix:", nrow(X), "rows x", ncol(X), "features\n")

# -----------------------------------------------------------------------
# Model selection: elbow method (k = 1..10)
# -----------------------------------------------------------------------
k_range <- 1:10
set.seed(602)
wss <- sapply(k_range, function(k) kmeans(X, centers = k, nstart = 10, iter.max = 50)$tot.withinss)
elbow_df <- data.frame(k = k_range, wss = wss)
elbow_df$pct_drop <- c(NA, round(100 * diff(-elbow_df$wss) / head(-elbow_df$wss, -1), 1))
write.csv(elbow_df, "outputs/data/elbow_results.csv", row.names = FALSE)
cat("\n=== Elbow method: total WSS by k ===\n")
print(elbow_df)

# -----------------------------------------------------------------------
# Model selection: silhouette method (evaluated on an 8,000-row sample --
# silhouette is O(n^2) and infeasible on the full 100,000 rows; cluster
# assignments themselves still come from the full-data K-means fit)
# -----------------------------------------------------------------------
set.seed(602)
samp_idx <- sample(nrow(X), 8000)
Xs <- X[samp_idx, ]
sil_width <- sapply(2:10, function(k) {
  km <- kmeans(X, centers = k, nstart = 10, iter.max = 50)
  mean(silhouette(km$cluster[samp_idx], dist(Xs))[, 3])
})
sil_df <- data.frame(k = 2:10, avg_silhouette = round(sil_width, 4))
write.csv(sil_df, "outputs/data/silhouette_results.csv", row.names = FALSE)
cat("\n=== Silhouette method: average width by k ===\n")
print(sil_df)
cat("\nElbow flattens from k=6 onward; silhouette's local peak (0.286) is at k=6-7,\n")
cat("close behind the global max at k=2 (0.293), which is too coarse for a business\n")
cat("segmentation. k=6 was chosen -- see README for the full reasoning.\n")

# -----------------------------------------------------------------------
# Final K-means fit (k = 6)
# -----------------------------------------------------------------------
set.seed(602)
km6 <- kmeans(X, centers = 6, nstart = 10, iter.max = 50)
raw_ord$cluster <- km6$cluster

risk_vars <- c("monthly_income","savings_balance","current_balance","total_outstanding_debt",
               "debt_to_income_ratio","credit_score","num_active_loans","num_missed_payments",
               "overdraft_usage_yearly")

profile <- raw_ord %>% group_by(cluster) %>%
  summarise(n = n(), pct = round(100 * n() / nrow(raw_ord), 1),
            across(all_of(risk_vars), ~round(mean(.x), 2)))
profile <- profile[order(profile$cluster), ]
write.csv(profile, "outputs/data/kmeans_cluster_profile.csv", row.names = FALSE)
cat("\n=== K-means (k=6) cluster profile ===\n")
print(as.data.frame(profile))

# -----------------------------------------------------------------------
# DBSCAN: complementary density-based pass to surface outliers K-means
# would otherwise average into their nearest cluster
# -----------------------------------------------------------------------
minPts <- 2 * ncol(X)   # standard rule of thumb: 2 x dimensionality
db <- dbscan(X, eps = 0.60, minPts = minPts)
raw_ord$dbscan_cluster <- db$cluster
cat("\n=== DBSCAN cluster sizes (0 = noise/outliers) ===\n")
print(table(db$cluster))

noise_flag <- db$cluster == 0
db_compare <- data.frame(
  variable = risk_vars,
  noise_mean = round(sapply(risk_vars, function(v) mean(raw_ord[[v]][noise_flag])), 2),
  clustered_mean = round(sapply(risk_vars, function(v) mean(raw_ord[[v]][!noise_flag])), 2)
)
write.csv(db_compare, "outputs/data/dbscan_noise_comparison.csv", row.names = FALSE)
cat("\n=== DBSCAN noise vs. clustered customers (raw risk indicators) ===\n")
print(db_compare)

# -----------------------------------------------------------------------
# Figures
# -----------------------------------------------------------------------
p1 <- ggplot(elbow_df, aes(k, wss)) +
  geom_line(color = "#2C6E9E", linewidth = 1) + geom_point(size = 2, color = "#2C6E9E") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "K-means elbow method", x = "Number of clusters (k)", y = "Total within-cluster sum of squares") +
  theme_minimal(base_size = 13)
ggsave("outputs/figures/elbow_plot.png", p1, width = 7, height = 4.5, dpi = 150)

p2 <- ggplot(sil_df, aes(k, avg_silhouette)) +
  geom_line(color = "#C24D2C", linewidth = 1) + geom_point(size = 2, color = "#C24D2C") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "grey40") +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "K-means silhouette method", x = "Number of clusters (k)", y = "Average silhouette width (n=8,000 sample)") +
  theme_minimal(base_size = 13)
ggsave("outputs/figures/silhouette_plot.png", p2, width = 7, height = 4.5, dpi = 150)

size_df <- profile %>% mutate(cluster = factor(cluster))
p3 <- ggplot(size_df, aes(x = cluster, y = n, fill = cluster)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(n, " (", pct, "%)")), vjust = -0.3, size = 3.5) +
  labs(title = "K-means (k=6) cluster sizes", x = "Cluster", y = "Number of customers") +
  theme_minimal(base_size = 13)
ggsave("outputs/figures/cluster_sizes.png", p3, width = 7, height = 4.5, dpi = 150)

pca <- prcomp(X, center = FALSE, scale. = FALSE)
var_exp <- round(100 * (pca$sdev^2 / sum(pca$sdev^2))[1:2], 1)
plot_df <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2],
                       kmeans_cluster = factor(km6$cluster),
                       dbscan_cluster = factor(ifelse(db$cluster == 0, "Noise", db$cluster)))
set.seed(602)
plot_samp <- plot_df[sample(nrow(plot_df), 12000), ]

p4 <- ggplot(plot_samp, aes(PC1, PC2, color = kmeans_cluster)) +
  geom_point(alpha = 0.4, size = 0.8) +
  labs(title = "K-means (k=6) clusters - PCA projection",
       x = paste0("PC1 (", var_exp[1], "%)"), y = paste0("PC2 (", var_exp[2], "%)"), color = "Cluster") +
  theme_minimal(base_size = 13)
ggsave("outputs/figures/kmeans_pca.png", p4, width = 7.5, height = 5.5, dpi = 150)

p5 <- ggplot(plot_samp, aes(PC1, PC2, color = dbscan_cluster)) +
  geom_point(alpha = 0.4, size = 0.8) +
  scale_color_manual(values = c("1"="#2C6E9E","2"="#C24D2C","3"="#3A9E4D","4"="#8E5EA2","5"="#D4A017","Noise"="grey70")) +
  labs(title = "DBSCAN clusters - PCA projection (grey = noise/outliers)",
       x = paste0("PC1 (", var_exp[1], "%)"), y = paste0("PC2 (", var_exp[2], "%)"), color = "Cluster") +
  theme_minimal(base_size = 13)
ggsave("outputs/figures/dbscan_pca.png", p5, width = 7.5, height = 5.5, dpi = 150)

# -----------------------------------------------------------------------
# Final per-customer cluster assignments
# -----------------------------------------------------------------------
assignments <- data.frame(
  customer_id = features$customer_id,
  kmeans_cluster = km6$cluster,
  dbscan_cluster = db$cluster
)
write.csv(assignments, "outputs/data/cluster_assignments.csv", row.names = FALSE)
cat("\nDone. Figures in outputs/figures/, tables in outputs/data/\n")
