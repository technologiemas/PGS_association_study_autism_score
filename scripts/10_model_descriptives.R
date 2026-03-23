# This script produces descriptive statistics for the model results

rm(list = ls(all = TRUE))
gc()

library(psych)
library(broom.mixed)

fit_m3 <- readRDS("results/models/fit_m3_clmm.rds")
fit_m4 <- readRDS("results/models/fit_m4_clmm.rds")
fit_m5 <- readRDS("results/models/fit_m5_clmm.rds")
mf_3 <- model.frame(fit_m3)
mf_4 <- model.frame(fit_m4)
mf_5 <- model.frame(fit_m5)

psych::describe(mf_3)
# describe for male and female separately
psych::describeBy(mf_3,
                  group = mf_3$sex,
                  mat = TRUE)


# score distribution by sex and rater type
by(mf_3$autism_score_ordinal, list(mf_3$rater_type, mf_3$sex), summary)

# function to get autism score level distribution
get_score_distribution <- function(model_frame) {
  # score distribution by sex and rater type (as a tidy table)
  distribution_scores_long <- as.data.frame(
    with(model_frame, table(rater_type, sex, autism_score_ordinal)),
    responseName = "n"
  )

  # optional: wide format (one row per rater_type + sex, columns: no/mild/high)
  distribution_scores_df <- reshape(
    distribution_scores_long,
    idvar = c("rater_type", "sex"),
    timevar = "autism_score_ordinal",
    direction = "wide"
  )

  names(distribution_scores_df) <- sub("^n\\.", "", names(distribution_scores_df))
  distribution_scores_df <- distribution_scores_df[order(distribution_scores_df$rater_type, distribution_scores_df$sex), ]
  return(distribution_scores_df)
}

# ORs and 95% CIs of all model estimates
list_model_results = function(model) {
  tidy(model) %>%
    mutate(
      estimate = estimate,
      SE = std.error,
      `Odds Ratio` = exp(estimate),
      `OR 95% CI Lower` = exp(estimate - 1.96 * std.error),
      `OR 95% CI Upper` = exp(estimate + 1.96 * std.error),
      p_value = p.value,
      z_value = statistic
    ) %>%
    select(term, estimate, SE, `Odds Ratio`, `OR 95% CI Lower`, `OR 95% CI Upper`, p_value, z_value)
}

distribution_scores_m3 <- get_score_distribution(mf_3)
distribution_scores_m4 <- get_score_distribution(mf_4)
distribution_scores_m5 <- get_score_distribution(mf_5)

model_results_m3 <- list_model_results(fit_m3)
model_results_m4 <- list_model_results(fit_m4)
model_results_m5 <- list_model_results(fit_m5)

lst_results <- list(
  "model_3_output" = model_results_m3,
  "model_4_output" = model_results_m4,
  "model_5_output" = model_results_m5,
  "autism_scores_lvls_distribution" = distribution_scores_m3
)

openxlsx::write.xlsx(lst_results, "results/model_output.xlsx") 


