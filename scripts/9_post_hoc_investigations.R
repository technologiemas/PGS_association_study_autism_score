# This script performs post-hoc investigations on the fitted clmm models from the main analyses
# Investigates two-way and three-way interaction effects using estimated marginal means and trends using the emmeans package

rm(list = ls(all = TRUE))
gc()

library(ordinal)
library(emmeans)
library(dplyr)
library(openxlsx)

source("scripts/_10_functions.R") # source the functions from 10_post_hoc_sensitivity.R

# Load the the clmm model with the best fit (model 3 & 4)
fit_m3 <- restore_ylevel_names(readRDS("results/models/fit_m3_clmm.rds"))
fit_m4 <- restore_ylevel_names(readRDS("results/models/fit_m4_clmm.rds"))
fit_m5 <- restore_ylevel_names(readRDS("results/models/fit_m5_clmm.rds"))

fit_m3_sensitivity <- restore_ylevel_names(readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds"))
fit_m4_sensitivity <- restore_ylevel_names(readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds"))

fit_m3_sensitivity_all_raters <- restore_ylevel_names(readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds"))
fit_m4_sensitivity_all_raters <- restore_ylevel_names(readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds"))

fit_ysr_12 = restore_ylevel_names(readRDS("results/models/sensitivity/fit_ysr_12_clmm_sensitivity.rds")) # with the ysr at age 12 included


# --- Two way interaction effects post hoc investigations ---

# estimated marginal means for rater type within each sex
get_emm_rater_sex <- function(fit_model, at = list(PGS_scaled = 0)) {
  emmeans(fit_model, ~ rater * sex_effect,
    at = at, # this averages over the specified values for the estimated marginal means (default is at the mean of PGS (0))
    mode = "latent",
    cov.reduce = mean
  ) # results are averaged over levels of categorical variables (such as PLATFORM) and at the mean of numeric variables (such as PGS = 0)
}

# estimated probabilities for individual autism score levels within each rater type and sex
get_eprob_rater_sex <- function(fit_model, outcome_var) {
  formula_str <- paste("~ sex_effect *", outcome_var, "| rater")
  emmeans(fit_model, as.formula(formula_str), # "within each rater what is the prob for the autism score levels?"
    mode = "prob",
    cov.reduce = mean # this averages over PGS and other covariates
  )
}

# --- Three way interaction effect post hoc investigation ---

# three way PGS * sex * rater: slopes of PGS predicting autism score ordinal within each rater type and sex
get_slopes_three_way <- function(fit_model) {
  emtrends(
    fit_model,
    ~ rater * sex_effect,
    var = "PGS_scaled",
    mode = "latent"
  )
}

# collect results and save as excel files
lst_results <- function(two_way_model, three_way_model, outcome_var) {
  list(
    `M3 EMM` = summary(get_emm_rater_sex(two_way_model), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(), 
    
    `M3 contrasts sexes` =
      contrast(
        get_emm_rater_sex(two_way_model),
        method = "pairwise",
        by = "rater",
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_columns(),

    `M3 contrasts 2SD PGS` =
      contrast(
        get_emm_rater_sex(two_way_model, at = list(PGS_scaled = 2)), # this is the contrast at 2SD above the mean of PGS
        method = "pairwise",
        by = "rater",
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_columns(),

    # pairwise contrasts between rater types within each sex
    `M3 contrasts raters` =
      contrast(
        get_emm_rater_sex(two_way_model),
        method = "pairwise",
        by = "sex_effect",
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_columns(),

    # cumulative probabilities for each autism score level (no, low, high) within each rater type and sex
    `M3 probs` =
      summary(get_eprob_rater_sex(two_way_model, outcome_var), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(),

    # pairwise contrasts between rater types within each sex for the estimated probabilities
    `M3 probs sexes` =
      contrast(
        get_eprob_rater_sex(two_way_model, outcome_var),
        method = "pairwise",
        by = c("rater", outcome_var),
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_prob_columns(),
      
    `M3 probs raters` =
      contrast(
        get_eprob_rater_sex(two_way_model, outcome_var),
        method = "pairwise",
        by = c("sex_effect", outcome_var),
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_prob_columns(),

    # pgs association with latent autism score ordinal within each rater type and sex
    `M4 PGS emtrends` =
      summary(get_slopes_three_way(three_way_model), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(),

    # pairwise contrast between sexes of pgs association with probability of autism score ordinal within each rater type
    `M4 PGS sexes` =
      contrast(
        get_slopes_three_way(three_way_model),
        method = "pairwise",
        by = "rater",
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_columns(),

    # pairwise contrast between rater types of pgs association with probability of autism score ordinal within
    `M4 PGS raters` =
      contrast(
        get_slopes_three_way(three_way_model),
        method = "pairwise",
        by = "sex_effect",
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_columns()
  )
}

# a function to remove all degrees of freedom (df) columns as these are nonsensical for clmm models (all show up as NA and inf)
drop_df_columns <- function(list_of_tables) {
  lapply(list_of_tables, function(table) {
    table %>%
      select(-contains("df"))
  })
}

post <- lst_results(fit_m3, fit_m4, "autism_score_ordinal")
post_sensitivity <- lst_results(fit_m3_sensitivity, fit_m4_sensitivity, "autism_score_ordinal_sensitivity")
post_sensitivity_all_raters <- lst_results(fit_m3_sensitivity_all_raters, fit_m4_sensitivity_all_raters, "autism_score_ordinal")
post_ysr_12 = contrast(emmeans(fit_ysr_12, ~ sex_effect), method = "pairwise", adjust = "none") %>% calc_fdr_p() %>% rename_columns()

# remove df columns which are not meaningful for clmm models
post <- drop_df_columns(post)
post_sensitivity <- drop_df_columns(post_sensitivity)
post_sensitivity_all_raters <- drop_df_columns(post_sensitivity_all_raters)
post_ysr_12 <- post_ysr_12 %>% select(-contains("df"))

# rename sheets in post, post_sensitivity, and post_sensitivity_all_raters to have a postfix indicating the dataset
names(post_sensitivity) <- paste0("Sens - ",names(post_sensitivity))
names(post_sensitivity_all_raters) <- paste0("Sens all - ",names(post_sensitivity_all_raters))

# save results
dir.create("results/post_hoc_investigations", showWarnings = FALSE, recursive = TRUE) # create directory if it doesn't exist
openxlsx::write.xlsx(post, "results/post_hoc_investigations/post.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity, "results/post_hoc_investigations/post_sensitivity.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity_all_raters, "results/post_hoc_investigations/post_sensitivity_all_raters.xlsx") # save as xlsx
openxlsx::write.xlsx(post_ysr_12, "results/post_hoc_investigations/post_ysr_12.xlsx") # save as xlsx

