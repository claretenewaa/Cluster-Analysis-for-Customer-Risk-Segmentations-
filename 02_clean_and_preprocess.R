# =============================================================================
# 02_clean_and_preprocess.R
#
# Cleans the raw synthetic dataset and prepares a scaled feature matrix
# for clustering.
#
# Variable selection: nine variables were chosen to represent three
# complementary dimensions of financial risk:
#   - repayment capacity: monthly_income, savings_balance, current_balance
#   - leverage: total_outstanding_debt, debt_to_income_ratio, num_active_loans
#   - repayment behaviour: credit_score, num_missed_payments, overdraft_usage_yearly
#
# Demographic fields were deliberately excluded from the clustering variables
# (not direct risk indicators, and raise proxy-discrimination concerns).
# Product-flag variables already summarised by num_active_loans, and
# transaction variables that mostly reflect channel activity rather than
# risk, were excluded as redundant.
#
# Run after 01_generate_data.R.
# =============================================================================

suppressMessages(library(dplyr))

df <- readRDS("data/bank_data_raw.rds")
cat("Starting rows:", nrow(df), "\n")

clust_vars <- c("customer_id","monthly_income","savings_balance","current_balance",
                 "credit_score","num_active_loans","total_outstanding_debt",
                 "debt_to_income_ratio","num_missed_payments","overdraft_usage_yearly")

# -----------------------------------------------------------------------
# Issue 1: exact duplicate records (data-integration duplication)
# -----------------------------------------------------------------------
n_dup <- sum(duplicated(df))
df <- df %>% distinct()
cat(sprintf("[Issue 1] Removed %d exact duplicate rows -> %d rows remain\n", n_dup, nrow(df)))

# -----------------------------------------------------------------------
# Issue 2: implausible / impossible values (entry errors)
# -----------------------------------------------------------------------
n_income_bad <- sum(df$monthly_income <= 0 | df$monthly_income > 30000, na.rm = TRUE)
df$monthly_income[df$monthly_income <= 0] <- NA
df$monthly_income[df$monthly_income > 30000] <- NA
cat(sprintf("[Issue 2a] Recoded %d implausible monthly_income values to NA\n", n_income_bad))

n_dti_neg <- sum(df$debt_to_income_ratio < 0, na.rm = TRUE)
df$debt_to_income_ratio <- abs(df$debt_to_income_ratio)
cat(sprintf("[Issue 2b] Corrected %d negative debt_to_income_ratio values via abs()\n", n_dti_neg))

sb_cap <- as.numeric(quantile(df$savings_balance, 0.995, na.rm = TRUE))
n_sb_out <- sum(df$savings_balance > sb_cap, na.rm = TRUE)
df$savings_balance <- pmin(df$savings_balance, sb_cap)
cat(sprintf("[Issue 2c] Winsorised %d extreme savings_balance values at the 99.5th pct cap (%.0f)\n",
            n_sb_out, sb_cap))

# -----------------------------------------------------------------------
# Issue 3: missing values (median imputation + missingness flags)
# -----------------------------------------------------------------------
n_miss_income <- sum(is.na(df$monthly_income))
n_miss_credit <- sum(is.na(df$credit_score))

df$income_was_missing <- as.integer(is.na(df$monthly_income))
df$credit_was_missing <- as.integer(is.na(df$credit_score))

df$monthly_income[is.na(df$monthly_income)] <- median(df$monthly_income, na.rm = TRUE)
df$credit_score[is.na(df$credit_score)]     <- median(df$credit_score, na.rm = TRUE)

cat(sprintf("[Issue 3] Median-imputed %d missing monthly_income and %d missing credit_score values\n",
            n_miss_income, n_miss_credit))

cat("\nRemaining NAs in clustering variables:\n")
print(sapply(df[clust_vars], function(x) sum(is.na(x))))

saveRDS(df, "data/bank_data_clean.rds")
cat("\nSaved data/bank_data_clean.rds (", nrow(df), "rows )\n")

# =============================================================================
# Preprocessing: transform and scale for clustering
# =============================================================================
skew <- function(x){ m <- mean(x); s <- sd(x); mean((x - m)^3) / s^3 }

# Monetary variables: heavily right-skewed -> log1p, then Z-score
log_vars <- c("monthly_income","savings_balance","current_balance",
              "total_outstanding_debt","debt_to_income_ratio")

cat("\nSkewness BEFORE log transform:\n")
for (v in log_vars) cat(sprintf("  %-25s %.2f\n", v, skew(df[[v]])))

df_log <- df
for (v in log_vars) df_log[[paste0(v, "_log")]] <- log1p(df_log[[v]])

cat("\nSkewness AFTER log1p transform:\n")
for (v in log_vars) cat(sprintf("  %-25s %.2f\n", v, skew(df_log[[paste0(v,"_log")]])))

z_scale <- function(x) as.numeric(scale(x))

# credit_score: symmetric spread, non-zero IQR -> robust (median/IQR) scaling
robust_scale <- function(x) {
  med <- median(x); iqr <- IQR(x)
  (x - med) / iqr
}

# Zero-inflated count variables: median AND IQR are both 0 for these,
# which makes robust scaling degenerate -> Min-Max normalisation instead
minmax_vars <- c("num_active_loans","num_missed_payments","overdraft_usage_yearly")
minmax_scale <- function(x) (x - min(x)) / (max(x) - min(x))

features <- data.frame(customer_id = df_log$customer_id)
for (v in log_vars) features[[paste0(v, "_z")]] <- z_scale(df_log[[paste0(v, "_log")]])
features[["credit_score_robust"]] <- robust_scale(df_log[["credit_score"]])
for (v in minmax_vars) features[[paste0(v, "_minmax")]] <- minmax_scale(df_log[[v]])

cat("\nFinal feature matrix:", nrow(features), "rows x", ncol(features) - 1, "features\n")

saveRDS(features, "data/cluster_features.rds")
write.csv(features, "data/cluster_features.csv", row.names = FALSE)
cat("Saved data/cluster_features.csv\n")
