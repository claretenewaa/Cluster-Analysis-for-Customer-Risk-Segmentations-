# =============================================================================
# 01_generate_data.R
#
# Generates a synthetic retail-banking customer dataset (~100,000 records)
# used throughout this project. All variables and relationships are
# artificial but designed to resemble patterns found in real banking data:
# demographics, product holdings, balances, transaction behaviour, credit
# risk indicators, and customer engagement. Realistic data-quality issues
# (missing values, entry errors, duplicate rows) are intentionally embedded
# so the cleaning pipeline (02_clean_and_preprocess.R) has real problems to
# solve, rather than working on already-tidy data.
#
# Run this first. It writes data/bank_data_raw.rds for the next script.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(lubridate)
})

generate_synthetic_bank_data <- function(n = 100000, seed = 602) {
  set.seed(seed)

  # ── SECTION 1: IDENTIFIERS ─────────────────────────────────────────────
  customer_id   <- 1:n
  branch_id     <- sample(1:300, n, replace = TRUE)
  relationship_manager_id <- sample(1:800, n, replace = TRUE)
  account_open_date <- as.Date("2024-12-31") -
    days(round(rlnorm(n, meanlog = 6.5, sdlog = 1.0)))

  # ── SECTION 2: DEMOGRAPHICS ────────────────────────────────────────────
  age <- pmax(18, pmin(85, round(rnorm(n, mean = 41, sd = 13))))

  gender <- sample(c("Male", "Female", "Other"), n, replace = TRUE,
                    prob = c(0.48, 0.50, 0.02))

  region <- sample(
    c("Greater Accra", "Ashanti", "Western", "Eastern",
      "Northern", "Volta", "Central", "Bono"),
    n, replace = TRUE,
    prob = c(0.30, 0.20, 0.12, 0.10, 0.10, 0.08, 0.06, 0.04)
  )

  education <- sample(
    c("None", "Primary", "Secondary", "Tertiary", "Postgraduate"),
    n, replace = TRUE,
    prob = c(0.04, 0.12, 0.32, 0.38, 0.14)
  )

  employment_status <- sample(
    c("Salaried", "Self-Employed", "Business Owner", "Student", "Unemployed", "Retired"),
    n, replace = TRUE,
    prob = c(0.42, 0.20, 0.13, 0.08, 0.07, 0.10)
  )

  marital_status <- sample(
    c("Single", "Married", "Divorced", "Widowed"),
    n, replace = TRUE,
    prob = c(0.42, 0.45, 0.08, 0.05)
  )

  dependents <- pmax(0, round(rpois(n, lambda = ifelse(
    marital_status == "Married", 2.0, 0.5
  ))))

  base_income <- case_when(
    education == "None"          ~  600,
    education == "Primary"       ~ 1000,
    education == "Secondary"     ~ 2200,
    education == "Tertiary"      ~ 4500,
    education == "Postgraduate"  ~ 8000,
    TRUE ~ 2200
  )

  monthly_income <- round(
    base_income *
      case_when(
        employment_status == "Salaried"       ~ 1.00,
        employment_status == "Self-Employed"  ~ 1.15,
        employment_status == "Business Owner" ~ 1.45,
        employment_status == "Student"        ~ 0.25,
        employment_status == "Unemployed"     ~ 0.15,
        employment_status == "Retired"        ~ 0.65,
        TRUE ~ 1.00
      ) *
      ifelse(region %in% c("Greater Accra", "Ashanti"), 1.30, 1.00) *
      rlnorm(n, meanlog = 0, sdlog = 0.40),
    2
  )

  # ── SECTION 3: ACCOUNT & PRODUCT HOLDINGS ──────────────────────────────
  account_type <- sample(
    c("Basic", "Standard", "Premium", "Private Banking"),
    n, replace = TRUE,
    prob = c(0.40, 0.35, 0.18, 0.07)
  )

  has_savings_account <- rbinom(n, 1, prob = 0.95)

  has_current_account <- rbinom(n, 1, prob = plogis(
    -1.0 + 0.9 * (account_type %in% c("Standard","Premium","Private Banking")) +
      0.3 * (employment_status %in% c("Salaried","Business Owner"))
  ))

  has_fixed_deposit <- rbinom(n, 1, prob = plogis(
    -2.0 + 0.7 * (account_type %in% c("Premium","Private Banking")) +
      0.5 * (log1p(monthly_income) > 8)
  ))

  has_credit_card <- rbinom(n, 1, prob = plogis(
    -2.2 + 1.0 * (account_type %in% c("Standard","Premium","Private Banking")) +
      0.4 * (employment_status == "Salaried") +
      0.3 * (age > 28)
  ))

  has_personal_loan <- rbinom(n, 1, prob = plogis(
    -2.0 + 0.5 * (employment_status %in% c("Salaried","Business Owner")) +
      0.3 * (dependents > 1) -
      0.4 * (account_type == "Private Banking")
  ))

  has_mortgage <- rbinom(n, 1, prob = plogis(
    -3.5 + 0.8 * (age > 30 & age < 60) +
      0.6 * (log1p(monthly_income) > 8.5) +
      0.5 * (marital_status == "Married")
  ))

  has_investment_account <- rbinom(n, 1, prob = plogis(
    -2.5 + 1.0 * (account_type %in% c("Premium","Private Banking")) +
      0.5 * (education %in% c("Tertiary","Postgraduate"))
  ))

  has_insurance_product <- rbinom(n, 1, prob = plogis(
    -1.8 + 0.5 * (has_mortgage == 1) +
      0.4 * (dependents > 0) +
      0.3 * (account_type != "Basic")
  ))

  has_mobile_banking <- rbinom(n, 1, prob = plogis(
    -0.5 + 0.6 * (age < 45) +
      0.4 * (education %in% c("Tertiary","Postgraduate")) +
      0.3 * (region %in% c("Greater Accra","Ashanti"))
  ))

  has_overdraft_facility <- rbinom(n, 1, prob = plogis(
    -2.5 + 0.7 * (account_type %in% c("Premium","Private Banking")) +
      0.4 * (employment_status == "Business Owner")
  ))

  # ── SECTION 4: BALANCES AND TRANSACTIONS ───────────────────────────────
  savings_balance <- round(
    pmax(0, monthly_income) *
      runif(n, 0.5, 8) *
      case_when(
        account_type == "Basic"           ~ 0.4,
        account_type == "Standard"        ~ 1.0,
        account_type == "Premium"         ~ 2.5,
        account_type == "Private Banking" ~ 6.0,
        TRUE ~ 1.0
      ) *
      rlnorm(n, 0, 0.6),
    2
  )

  current_balance <- ifelse(
    has_current_account == 1,
    round(monthly_income * runif(n, 0.2, 2.0) * rlnorm(n, 0, 0.5), 2),
    0
  )

  avg_monthly_transactions <- rpois(n, lambda = pmax(1,
                                                      3 + 0.002 * monthly_income +
                                                        5 * has_mobile_banking +
                                                        3 * (account_type %in% c("Premium","Private Banking"))
  ))

  avg_transaction_value <- round(
    pmax(5, monthly_income / pmax(1, avg_monthly_transactions) *
           rlnorm(n, 0, 0.4)),
    2
  )

  atm_withdrawals_monthly <- rpois(n, lambda = pmax(0.2,
                                                     4 - 1.5 * has_mobile_banking + 0.5 * (age > 50)
  ))

  online_transactions_monthly <- rpois(n, lambda = pmax(0,
                                                         2 + 8 * has_mobile_banking +
                                                           0.001 * monthly_income -
                                                           0.05 * age
  ))

  international_transactions_yearly <- rpois(n, lambda = pmax(0,
                                                               0.2 + 0.0008 * monthly_income +
                                                                 1.5 * (account_type %in% c("Premium","Private Banking"))
  ))

  overdraft_usage_yearly <- ifelse(
    has_overdraft_facility == 1,
    rpois(n, lambda = pmax(0, 2 + 0.3 * (employment_status=="Business Owner"))),
    0
  )

  # ── SECTION 5: CREDIT & RISK PROFILE ────────────────────────────────────
  credit_score_base <- 650 +
    1.5 * (log1p(monthly_income) - 7) * 20 +
    0.3 * age -
    30 * has_overdraft_facility -
    15 * overdraft_usage_yearly +
    rnorm(n, 0, 40)

  credit_score <- round(pmax(300, pmin(850, credit_score_base)))

  num_active_loans <- has_personal_loan + has_mortgage +
    rbinom(n, 1, prob = plogis(-2 + 0.3*(employment_status=="Business Owner")))

  total_outstanding_debt <- round(
    pmax(0,
         has_personal_loan * runif(n, 500, 15000) +
           has_mortgage      * runif(n, 20000, 250000) +
           has_overdraft_facility * overdraft_usage_yearly * runif(n, 100, 2000)
    ),
    2
  )

  debt_to_income_ratio <- round(
    total_outstanding_debt / pmax(1, monthly_income * 12),
    3
  )

  num_missed_payments <- rpois(n, lambda = pmax(0,
                                                0.3 + 0.02 * debt_to_income_ratio * 10 -
                                                  0.005 * (credit_score - 600)
  ))

  default_prob <- plogis(
    -3.0 +
      0.04 * num_missed_payments +
      1.5  * (debt_to_income_ratio > 0.5) -
      0.01 * (credit_score - 600) +
      0.3  * (employment_status %in% c("Unemployed","Student"))
  )
  loan_default_flag <- rbinom(n, 1, prob = pmin(default_prob, 0.6))

  # ── SECTION 6: CUSTOMER BEHAVIOUR & ENGAGEMENT ──────────────────────────
  tenure_years <- round(as.numeric(as.Date("2024-12-31") - account_open_date) / 365.25, 2)

  satisfaction_score <- pmin(10, pmax(1, round(
    7.0 -
      0.5 * num_missed_payments +
      0.3 * has_mobile_banking +
      0.2 * (account_type %in% c("Premium","Private Banking")) +
      rnorm(n, 0, 1.5)
  )))

  branch_visits_yearly <- rpois(n, lambda = pmax(0,
                                                 6 - 4 * has_mobile_banking + 0.05 * age
  ))

  num_complaints <- rpois(n, lambda = pmax(0,
                                           0.2 + 0.3 * num_missed_payments +
                                             0.4 * (satisfaction_score < 4)
  ))

  churn_prob <- plogis(
    -2.0 -
      0.3 * (satisfaction_score - 5) +
      0.2 * num_complaints -
      0.3 * has_mobile_banking -
      0.05 * tenure_years +
      0.2 * (account_type == "Basic")
  )
  churned <- rbinom(n, 1, prob = pmin(churn_prob, 0.7))

  # ── SECTION 7: FREE-TEXT FEEDBACK ───────────────────────────────────────
  feedback_templates <- c(
    "The mobile app keeps crashing whenever I try to make a transfer.",
    "My loan application took far too long to be approved.",
    "Customer service at the branch was excellent and very helpful.",
    "I was charged a fee I did not understand on my statement.",
    "The new online banking interface is confusing to navigate.",
    "Staff at the branch were rude and unprofessional during my visit.",
    "I appreciate the quick response when I reported a lost card.",
    "Interest rates on savings accounts are too low compared to competitors.",
    "The ATM near my home is frequently out of cash.",
    "Setting up the fixed deposit was smooth and well explained.",
    "I have been waiting for a refund on a failed transaction for weeks.",
    "The credit card application process was straightforward and fast.",
    "There is no clear information about overdraft charges.",
    "I love the new budgeting feature in the mobile app.",
    "My relationship manager rarely responds to my emails."
  )

  customer_feedback <- sample(feedback_templates, n, replace = TRUE)
  customer_feedback[sample(1:n, size = round(n * 0.15))] <- NA_character_

  # ── SECTION 8: INTENTIONAL DATA QUALITY ISSUES ──────────────────────────
  monthly_income[sample(1:n, size = round(n * 0.06))] <- NA_real_
  credit_score[sample(1:n, size = round(n * 0.04))] <- NA_real_
  dependents[sample(1:n, size = round(n * 0.03))] <- NA_integer_
  satisfaction_score[sample(1:n, size = round(n * 0.05))] <- NA_real_

  age[sample(1:n, size = round(n * 0.001))] <- sample(c(1, 5, 130, 150), round(n*0.001), replace = TRUE)
  monthly_income[sample(1:n, size = round(n * 0.001))] <- sample(c(0, -500, 5000000), round(n*0.001), replace = TRUE)
  savings_balance[sample(1:n, size = round(n * 0.0015))] <-
    savings_balance[sample(1:n, size = round(n * 0.0015))] * 1000
  debt_to_income_ratio[sample(1:n, size = round(n * 0.0008))] <- -abs(
    debt_to_income_ratio[sample(1:n, size = round(n * 0.0008))]
  )

  gender_messy <- gender
  messy_idx <- sample(1:n, size = round(n * 0.02))
  gender_messy[messy_idx] <- case_when(
    gender_messy[messy_idx] == "Male"   ~ sample(c("male","MALE","M"), length(messy_idx), replace=TRUE),
    gender_messy[messy_idx] == "Female" ~ sample(c("female","FEMALE","F"), length(messy_idx), replace=TRUE),
    TRUE ~ gender_messy[messy_idx]
  )

  region_messy <- region
  messy_idx2 <- sample(1:n, size = round(n * 0.015))
  region_messy[messy_idx2] <- trimws(paste0(" ", region_messy[messy_idx2], "  "))

  bad_date_idx <- sample(1:n, size = round(n * 0.0005))
  account_open_date[bad_date_idx] <- as.Date("2026-01-01") + days(sample(1:300, length(bad_date_idx), replace=TRUE))

  # ── ASSEMBLE FINAL DATA FRAME ────────────────────────────────────────────
  df <- data.frame(
    customer_id, branch_id, relationship_manager_id, account_open_date,
    age, gender = gender_messy, region = region_messy, education,
    employment_status, marital_status, dependents, monthly_income,
    account_type, has_savings_account, has_current_account, has_fixed_deposit,
    has_credit_card, has_personal_loan, has_mortgage, has_investment_account,
    has_insurance_product, has_mobile_banking, has_overdraft_facility,
    savings_balance, current_balance, avg_monthly_transactions,
    avg_transaction_value, atm_withdrawals_monthly, online_transactions_monthly,
    international_transactions_yearly, overdraft_usage_yearly,
    credit_score, num_active_loans, total_outstanding_debt,
    debt_to_income_ratio, num_missed_payments, loan_default_flag,
    tenure_years, satisfaction_score, branch_visits_yearly, num_complaints,
    churned, customer_feedback,
    stringsAsFactors = FALSE
  )

  dup_idx <- sample(1:nrow(df), size = round(nrow(df) * 0.003))
  df <- bind_rows(df, df[dup_idx, ])

  return(df)
}

df_raw <- generate_synthetic_bank_data(n = 100000, seed = 602)
cat("Generated dataset:", nrow(df_raw), "rows x", ncol(df_raw), "columns\n")

dir.create("data", showWarnings = FALSE)
saveRDS(df_raw, "data/bank_data_raw.rds")
cat("Saved data/bank_data_raw.rds\n")
