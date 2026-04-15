# post-hoc investigations on age and date of assessment three way interactions from file 06a_age_date.R
# This script performs post-hoc investigations on the fitted clmm models from the main analyses
# Investigates two-way and three-way interaction effects using estimated marginal means and trends using the emmeans package

rm(list = ls(all = TRUE))
gc()

library(emmeans)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ordinal)
library(ggplot2)

fit_age <- readRDS("results/models/sensitivity/age_three_way.rds")
fit_date <- readRDS("results/models/sensitivity/date_three_way.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")
vars_to_scale <- c("age", "date_of_assessment")

data_long = data_long %>%
  mutate(across(all_of(vars_to_scale),
                ~ as.numeric(scale(.)), .names = "{col}_scaled")) 


# calculate fdr adjusted p value
calc_fdr_p <- function(emm_obj, method = "fdr") {
  out = as.data.frame(emm_obj) %>%
    rename(any_of(c(
        "p_unadjusted"   = "p.value",
        "p_unadjusted"   = "p-value",
        "p_unadjusted"   = "p_value"
    )))

  out$p_fdr <- p.adjust(out$p_unadjusted, method = method)

  out
}

rename_columns <- function(emm_obj) {
  out <- as.data.frame(emm_obj)
  
  target_col <- intersect(c("estimate", "emmean", "age_scaled.trend", "date_of_assessment_scaled.trend"), names(out))[1]
  
  if (!is.na(target_col)) {
    out <- out %>%
      mutate(
        `Odds Ratio`      = exp(.data[[target_col]]),
        `OR 95% CI Lower` = exp(.data[[target_col]] - 1.96 * SE),
        `OR 95% CI Upper` = exp(.data[[target_col]] + 1.96 * SE)
      )
  }
  
  out <- out %>%
    rename(any_of(c(
      "p-value"                            = "p.value",
      "Estimated Probability"              = "prob",
      "Estimate difference (log-odds)"     = "estimate",
      "Estimated Marginal Mean (log-odds)" = "emmean",
      "Slope of Age (scaled) in log-odds"  = "age_scaled.trend",
      "Slope of Date of Assessment (scaled) in log-odds" = "date_of_assessment_scaled.trend"
    ))) %>%
    select(-contains("asymp.LCL"), -contains("asymp.UCL"))
  
  return(out)
}

# --- Three way interaction effect post hoc investigation ---

# three way PGS * sex * rater_type: slopes of PGS predicting autism score ordinal within each rater type and sex
get_slopes_three_way <- function(fit_model, var_for_slopes) {
  emtrends(
    fit_model,
    ~ rater_type * sex,
    var = var_for_slopes,
    mode = "latent"
  )
}

get_slopes_two_way <- function(fit_model, var_for_slopes) {
  emtrends(
    fit_model,
    ~ rater_type,
    var = var_for_slopes
  )
}

calculate_main_effects = function(fit_model, var_for_slopes) {
  emtrends(fit_model, ~ 1, var = var_for_slopes, mode = "latent")
}

# collect results and save as excel files
lst_results = function(three_way_model, outcome_var, var_for_slopes) {
  list(
  # pgs association with latent autism score ordinal within each rater type and sex
  # we use omnibus testing to test the overall significant of the two- and three-way interactions by using emmeans joint_tests()
  `predictors_omnibus_test` = joint_tests(three_way_model) %>% calc_fdr_p() %>% rename_columns(), 

  `main_effects` = summary(calculate_main_effects(three_way_model, var_for_slopes), infer = c(TRUE, TRUE)) %>% rename_columns(),

  `2_way_emtrends` = summary(get_slopes_two_way(three_way_model, var_for_slopes), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(),

  `3_way_emtrends` = summary(get_slopes_three_way(three_way_model, var_for_slopes), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(),

  # pairwise contrast between sexes of  with probability of autism score ordinal within each rater type
  `3_way_contrast_sex` =
    contrast(
      get_slopes_three_way(three_way_model, var_for_slopes),
      method = "pairwise",
      by = "rater_type",
    ) %>% calc_fdr_p() %>% rename_columns(),

  # pairwise contrast between rater types of  with probability of autism score ordinal within each rater type
  `3_way_contrast_rater` =
    contrast(
      get_slopes_three_way(three_way_model, var_for_slopes),
      method = "pairwise",
      by = "sex",
    ) %>% calc_fdr_p() %>% rename_columns()
)}

# a function to remove all degrees of freedom (df) columns as these are nonsensical for clmm models (all show up as NA and inf)
drop_df_columns <- function(list_of_tables) {
  lapply(list_of_tables, function(table) {
    table %>%
      select(-contains("df"))
  })
}

# we use omnibus testing to test the overall significant of the two- and three-way interactions by using emmeans joint_tests()
post_age = lst_results(fit_age, "autism_score_ordinal", "age_scaled")
post_date = lst_results(fit_date, "autism_score_ordinal", "date_of_assessment_scaled")

post_age <- drop_df_columns(post_age)
post_date <- drop_df_columns(post_date)

dir.create("results/post_hoc_investigations", showWarnings = FALSE, recursive = TRUE) # create directory if it doesn't exist
openxlsx::write.xlsx(post_age, "results/post_hoc_investigations/post_age.xlsx") # save as xlsx
openxlsx::write.xlsx(post_date, "results/post_hoc_investigations/post_date.xlsx") # save as xlsx



# make a ggplot of the linear three way relationship between age, sex and rater type predicting autism score 
ggplot(data_long, aes(x = age, y = autism_score, color = sex, alpha=0.3)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ rater_type) +
  labs(x = "Age", y = "Autism Score", title = "Linear relationship between age, sex and rater type predicting autism score") +
  theme(legend.position = "top") 

ggsave(
  "results/figures/age_three_way_relationship.png",
  width = 15, height = 10, units = "cm",
  dpi = 600, scale = 2
)

ggplot(data_long, aes(x = date_of_assessment, y = autism_score, color = sex, alpha=0.3)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ rater_type) +
  labs(x = "Date of Assessment", y = "Autism Score", title = "Linear relationship between date of assessment, sex and rater type predicting autism score") +
  theme(legend.position = "top")

ggsave(
    "results/figures/date_three_way_relationship.png",
    width = 15, height = 10, units = "cm",
    dpi = 600, scale = 2
)



a = emmeans(fit_age, ~ rater_type * sex, var = "age_scaled", mode = "latent")
b = contrast(a, method = "pairwise", by = "sex")
