#Question 1 why do we need log
## In the lecture, it mentioned there are multiple ways to fail to replicate the experients, even the 

install.packages(c("renv", "logger", "tidyverse", "broom"))

library(renv)       # Dependency management (renv.lock)
library(logger)     # Logging pipeline steps
library(tidyverse)  # Data manipulation + plotting
library(broom)      # Tidy regression outputs (for tables)
library(ggplot2)    #ggplot
#renv::init()
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
# Logging creates an audit trail:
# - What ran
# - In what order
# - With what parameters
# - Where outputs were written

logger::log_threshold(DEBUG)
logger::log_appender(appender_file("analysis_log.txt"))
set.seed(123)  # Reproducible randomness for the full pipeline

log_info("Starting analysis pipeline")

# Expected location for this assignment:
# - data/raw/education_income.csv

education_income_raw <- readr::read_csv("data/raw/education_income.csv")

log_info("Loading education/income dataset from data/raw/education_income.csv")



log_info(paste("Rows loaded:", nrow(education_income_raw)))
log_info(paste("Columns loaded:", ncol(education_income_raw)))

# In many projects, "raw" is treated as read-only and comes from outside.
# Here we re-write it to confirm the exact file used in the run.

log_info("Saving raw data copy (unchanged)")
# readr::write_csv(education_income_raw, "data/raw/education_income.csv")

# Keep this simple and explicit:
# - Ensure education and income exist
# - Coerce to numeric (if needed)
# - Drop missing
#
# Note: No if/else. If columns are missing, the script will error (which is fine).

log_info("Cleaning education/income data")

education_income_clean <- education_income_raw |>
  dplyr::mutate(
    education = as.numeric(education),
    income    = as.numeric(income)
  ) |>
  dplyr::filter(!is.na(education), !is.na(income))

log_info(paste("Rows after cleaning:", nrow(education_income_clean)))

# Create log-income version for Model 3
# If income has zeros or negatives, log(income) is not finite.
education_income_clean <- education_income_clean |>
  dplyr::mutate(log_income = log(income))

education_income_log <- education_income_clean |>
  dplyr::filter(is.finite(log_income))

log_info(paste("Rows with finite log(income):", nrow(education_income_log)))

log_info("Saving processed data")
readr::write_csv(education_income_clean, "data/processed/cleaned_education_income.csv")


# --------------------------
# Models + Save plots + Save tables
# --------------------------

log_info("Fitting Model 1: income ~ education")
model_1 <- lm(income ~ education, data = education_income_clean)

p1 <- ggplot(education_income_clean, aes(x = education, y = income)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Income vs Education",
    x = "Education",
    y = "Income"
  ) +
  theme_minimal()

ggsave("outputs/figures/model_1_income_vs_education.png",
       plot = p1, width = 7, height = 5, dpi = 300)
log_info("Wrote plot: outputs/figures/model_1_income_vs_education.png")


log_info("Fitting Model 2: income ~ education + I(education^2)")
model_2 <- lm(income ~ education + I(education^2), data = education_income_clean)

p2 <- ggplot(education_income_clean, aes(x = education, y = income)) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x^2),
    se = TRUE
  ) +
  labs(
    title = "Income vs Education (Quadratic Fit)",
    x = "Education",
    y = "Income"
  ) +
  theme_minimal()

ggsave("outputs/figures/model_2_quadratic_fit.png",
       plot = p2, width = 7, height = 5, dpi = 300)
log_info("Wrote plot: outputs/figures/model_2_quadratic_fit.png")


log_info("Fitting Model 3: log(income) ~ education (finite log income rows only)")
model_3 <- lm(log_income ~ education, data = education_income_log)

p3 <- ggplot(education_income_log, aes(x = education, y = log(income))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Log Income vs Education",
    x = "Education",
    y = "Log(Income)"
  ) +
  theme_minimal()

ggsave("outputs/figures/model_3_log_income_vs_education.png",
       plot = p3, width = 7, height = 5, dpi = 300)
log_info("Wrote plot: outputs/figures/model_3_log_income_vs_education.png")


# --------------------------
# Save model summaries (required)
# --------------------------
log_info("Saving regression summaries to outputs/tables/")

writeLines(capture.output(summary(model_1)), "outputs/tables/model_1_summary.txt")
log_info("Wrote: outputs/tables/model_1_summary.txt")

writeLines(capture.output(summary(model_2)), "outputs/tables/model_2_summary.txt")
log_info("Wrote: outputs/tables/model_2_summary.txt")

writeLines(capture.output(summary(model_3)), "outputs/tables/model_3_summary.txt")
log_info("Wrote: outputs/tables/model_3_summary.txt")


# --------------------------
# Create & write regression_coefficients.csv (required)
# --------------------------
log_info("Creating regression coefficients table")

coef_1 <- broom::tidy(model_1) |> dplyr::mutate(model = "Model 1: income ~ education")
coef_2 <- broom::tidy(model_2) |> dplyr::mutate(model = "Model 2: income ~ education + education^2")
coef_3 <- broom::tidy(model_3) |> dplyr::mutate(model = "Model 3: log(income) ~ education")

regression_coefficients <- dplyr::bind_rows(coef_1, coef_2, coef_3) |>
  dplyr::select(model, term, estimate, std.error, statistic, p.value)

readr::write_csv(regression_coefficients, "outputs/tables/regression_coefficients.csv")
log_info("Wrote: outputs/tables/regression_coefficients.csv")


# --------------------------
# Save sessionInfo() (required)
# --------------------------
log_info("Saving session information")
writeLines(capture.output(sessionInfo()), "outputs/session_info.txt")
log_info("Wrote: outputs/session_info.txt")


# --------------------------
# Snapshot dependencies (required)
# --------------------------
log_info("Snapshotting dependencies to renv.lock")
renv::snapshot()
log_info("Analysis pipeline completed successfully")