# This script performs post-hoc investigations on the fitted clmm models from the main analyses
# Investigates two-way and three-way interaction effects using estimated marginal means and trends using the emmeans package

rm(list = ls(all = TRUE))
gc()

library(ordinal)
library(emmeans)
library(dplyr)
library(openxlsx)
# library(gtsummary)

# Load the the clmm model with the best fit (model 3 & 4)
fit_m3 <- readRDS("results/models/fit_m3_clmm.rds")
fit_m4 <- readRDS("results/models/fit_m4_clmm.rds")
fit_m5 <- readRDS("results/models/fit_m5_clmm.rds")

fit_m3_sensitivity <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds") 
fit_m4_sensitivity <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds") 

fit_m3_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds") 
fit_m4_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds")

fit_m3_sensitivity_ysr_12 <- readRDS("results/models/sensitivity/fit_ysr_12_m3_clmm_sensitivity.rds") # TODO investigate this
fit_m4_sensitivity_ysr_12 <- readRDS("results/models/sensitivity/fit_ysr_12_m4_clmm_sensitivity.rds")


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

rename_prob_columns = function(emm_obj) {
  out <- as.data.frame(emm_obj)

  out <- out %>%
    mutate(
      `Probability Difference` = estimate,
      SE = SE,
      `Prob 95% CI Lower` = estimate - 1.96 * SE,
      `Prob 95% CI Upper` = estimate + 1.96 * SE
    ) %>%
    select(-estimate, -SE) # remove the original 'estimate' column as it's now represented as 'Probability'

  out
}

rename_columns <- function(emm_obj) {
  out <- as.data.frame(emm_obj)
  
  target_col <- intersect(c("estimate", "emmean", "PGS_scaled.trend"), names(out))[1]

  if (!is.na(target_col)) { # else it is a log-odds estimate
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
      "Slope of PGS (scaled) in log-odds"  = "PGS_scaled.trend"
    ))) %>%
    select(-contains("asymp.LCL"), -contains("asymp.UCL"))
  
  return(out)
}


# --- Two way interaction effects post hoc investigations ---

# estimated marginal means for rater type within each sex
get_emm_rater_sex = function(fit_model, at = list(PGS_scaled = 0)) {
  emmeans(fit_model, ~ rater_type * sex,
                          at = at, # this averages over the specified values for the estimated marginal means (default is at the mean of PGS (0))
                          mode = "latent",
                          cov.reduce = mean) # results are averaged over levels of categorical variables (such as PLATFORM) and at the mean of numeric variables (such as PGS = 0)
}

# estimated probabilities for individual autism score levels within each rater type and sex
get_eprob_rater_sex = function(fit_model, outcome_var) {
    formula_str <- paste("~ sex *", outcome_var, "| rater_type")
    emmeans(fit_model, as.formula(formula_str), # "within each rater what is the prob for the autism score levels?"
                  mode = "prob",
                  cov.reduce = mean # this averages over PGS and other covariates
                  )
}

# --- Three way interaction effect post hoc investigation ---

# three way PGS * sex * rater_type: slopes of PGS predicting autism score ordinal within each rater type and sex
get_slopes_three_way <- function(fit_model) {
  emtrends(
    fit_model,
    ~ rater_type * sex,
    var = "PGS_scaled",
    mode = "latent"
  )
} 

# collect results and save as excel files
lst_results = function(two_way_model, three_way_model, outcome_var) {
  list(

  emm = summary(get_emm_rater_sex(two_way_model), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(),

  contrast_emm_sex = 
    contrast(
      get_emm_rater_sex(two_way_model),
      method = "pairwise",
      by = "rater_type",
      adjust = "fdr"
    ) %>% calc_fdr_p() %>% rename_columns(),

  contrast_sex_pgs_2sd = 
    contrast(
      get_emm_rater_sex(two_way_model, at = list(PGS_scaled = 2)), # this is the contrast at 2SD above the mean of PGS
      method = "pairwise",
      by = "rater_type",
      adjust = "fdr"
    ) %>% calc_fdr_p() %>% rename_columns(),

  # pairwise contrasts between rater types within each sex
  contrast_emm_raters =
    contrast(
      get_emm_rater_sex(two_way_model),
      method = "pairwise",
      by = "sex",
    ) %>% calc_fdr_p() %>% rename_columns(),

  # cumulative probabilities for each autism score level (no, low, high) within each rater type and sex
  prob_rater_sex =
    summary(get_eprob_rater_sex(two_way_model, outcome_var), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(),

  # pairwise contrasts between rater types within each sex for the estimated probabilities
  contrast_prob_sex =
    contrast(
      get_eprob_rater_sex(two_way_model, outcome_var),
      method = "pairwise",
      by = c("rater_type", outcome_var),
    ) %>% calc_fdr_p() %>% rename_prob_columns(),
  
  contrast_prob_rater =
    contrast(
      get_eprob_rater_sex(two_way_model, outcome_var),
      method = "pairwise",
      by = c("sex", outcome_var),
    ) %>% calc_fdr_p() %>% rename_prob_columns(),

  # pgs association with latent autism score ordinal within each rater type and sex
  `3_way_emtrends` =
    summary(get_slopes_three_way(three_way_model), infer = c(TRUE, TRUE)) %>% calc_fdr_p() %>% rename_columns(), 

  # pairwise contrast between sexes of pgs association with probability of autism score ordinal within each rater type
  `3_way_contrast_sex` =
    contrast(
      get_slopes_three_way(three_way_model),
      method = "pairwise",
      by = "rater_type",
    ) %>% calc_fdr_p() %>% rename_columns(),

  # pairwise contrast between rater types of pgs association with probability of autism score ordinal within
  `3_way_contrast_rater` =
    contrast(
      get_slopes_three_way(three_way_model),
      method = "pairwise",
      by = "sex",
    ) %>% calc_fdr_p() %>% rename_columns()
)}


# We were interested in whether ratings were influenced by congruency between the sex of the rater and the sex of the participant.
# Here we calculate average of same-sex dyads minus average of cross-sex dyads
get_parent_child_dyad = function(model_fit) {
  emm_parent_subset <- emmeans(
    model_fit, 
    ~ rater_type * sex, 
    at = list(rater_type = c("Mother", "Father")),
    mode = "latent",
    cov.reduce = mean
  )
}

# a function to remove all degrees of freedom (df) columns as these are nonsensical for clmm models (all show up as NA and inf)
drop_df_columns <- function(list_of_tables) {
  lapply(list_of_tables, function(table) {
    table %>%
      select(-contains("df"))
  })
}

post = lst_results(fit_m3, fit_m4, "autism_score_ordinal")
post_sensitivity = lst_results(fit_m3_sensitivity, fit_m4_sensitivity, "autism_score_ordinal_sensitivity")
post_sensitivity_all_raters = lst_results(fit_m3_sensitivity_all_raters, fit_m4_sensitivity_all_raters, "autism_score_ordinal_sensitivity")
post_sensitivity_ysr_12 = lst_results(fit_m3_sensitivity_ysr_12, fit_m4_sensitivity_ysr_12, "autism_score_ordinal")

contrast_sex_dyads = contrast(get_parent_child_dyad(fit_m3), method = list("same_vs_cross" = c(-1, 1, 1, -1))) %>% calc_fdr_p() %>% rename_columns() # p value for whether incongruency effect exists
contrast_sex_dyads_sensitivity = contrast(get_parent_child_dyad(fit_m3_sensitivity), method = list("same_vs_cross" = c(-1, 1, 1, -1))) %>% calc_fdr_p() %>% rename_columns()
contrast_sex_dyads_sensitivity_all_raters = contrast(get_parent_child_dyad(fit_m3_sensitivity_all_raters), method = list("same_vs_cross" = c(-1, 1, 1, -1))) %>% calc_fdr_p() %>% rename_columns()
contrast_sex_dyads_sensitivity_ysr_12 = contrast(get_parent_child_dyad(fit_m3_sensitivity_ysr_12), method = list("same_vs_cross" = c(-1, 1, 1, -1))) %>% calc_fdr_p() %>% rename_columns()

# append contrast_sex_dyads in one table
all_contrast_sex_dyads = rbind(
  contrast_sex_dyads %>% mutate(dataset = "main"),
  contrast_sex_dyads_sensitivity %>% mutate(dataset = "sensitivity"),
  contrast_sex_dyads_sensitivity_all_raters %>% mutate(dataset = "sensitivity_all_raters"),
  contrast_sex_dyads_sensitivity_ysr_12 %>% mutate(dataset = "sensitivity_ysr_12")
) %>%
  select(dataset, everything()) # move dataset column to the front

# remove df columns which are not meaningful for clmm models
all_contrast_sex_dyads = select(all_contrast_sex_dyads, -contains("df"))
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
openxlsx::write.xlsx(all_contrast_sex_dyads, "results/post_hoc_investigations/contrast_sex_dyads.xlsx", sheetName = "contrast_sex_dyads") # save as xlsx
openxlsx::write.xlsx(post, "results/post_hoc_investigations/post.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity, "results/post_hoc_investigations/post_sensitivity.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity_all_raters, "results/post_hoc_investigations/post_sensitivity_all_raters.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity_ysr_12, "results/post_hoc_investigations/post_sensitivity_ysr_12.xlsx") # save as xlsx



# calculate main effect of PGS on autism score ordinal averaged across rater type, sex, other covariates 
# (as close to the main effect we can get in a model with categorical variables)
# for model 4
emtrends(fit_m4, ~ 1, var = "PGS_scaled", mode = "latent") %>% rename_columns()