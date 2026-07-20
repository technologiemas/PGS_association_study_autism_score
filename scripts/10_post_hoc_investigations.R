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
fit_m3 <- readRDS("results/models/fit_m3_clmm.rds")
fit_m4 <- readRDS("results/models/fit_m4_clmm.rds")
fit_m5 <- readRDS("results/models/fit_m5_clmm.rds")

fit_m3_sensitivity <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
fit_m4_sensitivity <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds")

fit_m3_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds")
fit_m4_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds")

fit_m3_sensitivity_ysr_12 <- readRDS("results/models/sensitivity/fit_ysr_12_m3_clmm_sensitivity.rds") # TODO investigate this


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
    `Model 3 EMM` = summary(get_emm_rater_sex(two_way_model), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(), 
    
    `Model 3 EMM contrasts sexes` =
      contrast(
        get_emm_rater_sex(two_way_model),
        method = "pairwise",
        by = "rater",
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_columns(),

    `Model 3 EMM contrasts 2SD PGS` =
      contrast(
        get_emm_rater_sex(two_way_model, at = list(PGS_scaled = 2)), # this is the contrast at 2SD above the mean of PGS
        method = "pairwise",
        by = "rater",
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_columns(),

    # pairwise contrasts between rater types within each sex
    `Model 3 EMM contrasts raters` =
      contrast(
        get_emm_rater_sex(two_way_model),
        method = "pairwise",
        by = "sex_effect",
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_columns(),

    # cumulative probabilities for each autism score level (no, low, high) within each rater type and sex
    `Model 3 probs` =
      summary(get_eprob_rater_sex(two_way_model, outcome_var), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(),

    # pairwise contrasts between rater types within each sex for the estimated probabilities
    `Model 3 probs contrast sexes` =
      contrast(
        get_eprob_rater_sex(two_way_model, outcome_var),
        method = "pairwise",
        by = c("rater", outcome_var),
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_prob_columns(),
      
    `Model 3 probs contrast raters` =
      contrast(
        get_eprob_rater_sex(two_way_model, outcome_var),
        method = "pairwise",
        by = c("sex_effect", outcome_var),
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_prob_columns(),

    # pgs association with latent autism score ordinal within each rater type and sex
    `Model 4 PGS emtrends` =
      summary(get_slopes_three_way(three_way_model), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(),

    # pairwise contrast between sexes of pgs association with probability of autism score ordinal within each rater type
    `Model 4 PGS contrast sexes` =
      contrast(
        get_slopes_three_way(three_way_model),
        method = "pairwise",
        by = "rater",
        adjust = "none"
      ) %>% calc_fdr_p() %>% rename_columns(),

    # pairwise contrast between rater types of pgs association with probability of autism score ordinal within
    `Model 4 PGS contrast raters` =
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
post_sensitivity_ysr_12 <- lst_results(fit_m3_sensitivity_ysr_12, fit_m4_sensitivity_ysr_12, "autism_score_ordinal")

# remove df columns which are not meaningful for clmm models
post <- drop_df_columns(post)
post_sensitivity <- drop_df_columns(post_sensitivity)
post_sensitivity_all_raters <- drop_df_columns(post_sensitivity_all_raters)
post_sensitivity_ysr_12 <- drop_df_columns(post_sensitivity_ysr_12)

# rename sheets in post, post_sensitivity, and post_sensitivity_all_raters to have a postfix indicating the dataset
names(post_sensitivity) <- paste0(names(post_sensitivity), "_sens")
names(post_sensitivity_all_raters) <- paste0(names(post_sensitivity_all_raters), "_sens_all")
names(post_sensitivity_ysr_12) <- paste0(names(post_sensitivity_ysr_12), "_sens_ysr12")

# save results
dir.create("results/post_hoc_investigations", showWarnings = FALSE, recursive = TRUE) # create directory if it doesn't exist
openxlsx::write.xlsx(post, "results/post_hoc_investigations/post.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity, "results/post_hoc_investigations/post_sensitivity.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity_all_raters, "results/post_hoc_investigations/post_sensitivity_all_raters.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity_ysr_12, "results/post_hoc_investigations/post_sensitivity_ysr_12.xlsx") # save as xlsx

