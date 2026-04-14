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


# helper function to get contrasts with unadjusted p-values. This function was made with help of AI but seems to work well
contrast_with_unadj <- function(emm_obj, ..., adjust = "fdr") {
  ctr <- contrast(emm_obj, ..., adjust = "none")

  unadj <- summary(ctr, adjust = "none")
  adj   <- summary(ctr, adjust = adjust)

  out <- as.data.frame(adj)
  out$p_unadjusted <- unadj$p.value

  names(out)[names(out) == "p.value"] <- paste0("p_", adjust)

  if(emm_obj@misc$estName == "prob") {
    out <- out %>%
      mutate(
        `Prob. Difference` = estimate,
        SE = SE,
        `Prob 95% CI Lower` = estimate - 1.96 * SE,
        `Prob 95% CI Upper` = estimate + 1.96 * SE
      ) %>%
      select(-estimate) # remove the original 'estimate' column as it's now represented as 'Probability'
  }

    # ---- Add odds ratios (only meaningful on latent/logit scale) ----
  if("estimate" %in% names(out)) {
    out$`Odds Ratio` <- exp(out$estimate)
    out$`OR 95% CI Lower` <- exp(out$estimate - 1.96 * out$SE)
    out$`OR 95% CI Upper` <- exp(out$estimate + 1.96 * out$SE)
  }

  if("asymp.LCL" %in% names(out)) {
    names(out)[names(out) == "asymp.LCL"] <- "95% CI Lower"
  }
  if("asymp.UCL" %in% names(out)) {
    names(out)[names(out) == "asymp.UCL"] <- "95% CI Upper"
  }

  out
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

calculate_main_effects = function(fit_model) {
  emmeans(fit_model, ~ 1, mode = "latent")
}

# collect results and save as excel files
lst_results = function(three_way_model, outcome_var, var_for_slopes) {
  list(
  # pgs association with latent autism score ordinal within each rater type and sex
  `predictors_omnibus_test` = joint_tests(three_way_model), # we use omnibus testing to test the overall significant of the two- and three-way interactions by using emmeans joint_tests()

  `main_effects` = summary(calculate_main_effects(three_way_model)),

  `2_way_emtrends` = summary(get_slopes_two_way(three_way_model, var_for_slopes), infer = c(TRUE, TRUE)),

  `3_way_emtrends` = summary(get_slopes_three_way(three_way_model, var_for_slopes), infer = c(TRUE, TRUE)),

  # pairwise contrast between sexes of  with probability of autism score ordinal within each rater type
  `3_way_contrast_sex` =
    contrast_with_unadj(
      get_slopes_three_way(three_way_model, var_for_slopes),
      method = "pairwise",
      by = "rater_type",
      adjust = "fdr"
    ),

  # pairwise contrast between rater types of  with probability of autism score ordinal within each rater type
  `3_way_contrast_rater` =
    contrast_with_unadj(
      get_slopes_three_way(three_way_model, var_for_slopes),
      method = "pairwise",
      by = "sex",
      adjust = "fdr"
    )
)}

# a function to remove all degrees of freedom (df) columns as these are nonsensical for clmm models (all show up as NA and inf)
drop_df_columns <- function(list_of_tables) {
  lapply(list_of_tables, function(table) {
    table %>%
      select(-contains("df"))
  })
}

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




# trying stuff here
  emtrends(
    fit_age,
    ~ rater_type * sex,
    var = "age_scaled",
    mode = "latent"
  )

# This gives the main effect of a variable (in this case age_scaled)
summary(emtrends(
  fit_age,
  ~ 1, 
  var = "age_scaled",
  mode = "latent"
), infer = c(TRUE, TRUE)
)
# This gives the main effect of a variable (in this case age_scaled)
summary(emtrends(
  fit_age,
  ~ 1, 
  mode = "latent"
), infer = c(TRUE, TRUE)
)

summary(emtrends(fit_age, ~ 1, mode = "latent", var = "age_scaled"), infer = c(TRUE, TRUE))
summary(emmeans(fit_age, ~ 1, mode = "latent", var = "rater_type"), infer = c(TRUE, TRUE))

# we use omnibus testing to test the overall significant of the two- and three-way interactions by using emmeans joint_tests()
joint_tests(fit_age)

