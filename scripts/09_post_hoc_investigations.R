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

fit_m3_sensitivity <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds") 
fit_m4_sensitivity <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds") 

fit_m3_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds") 
fit_m4_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds")

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
  # estimated marginal means for rater type within each sex
  estimated_marginal_means = as.data.frame(get_emm_rater_sex(two_way_model)),

  contrast_emm_sexes = 
    contrast_with_unadj(
      get_emm_rater_sex(two_way_model),
      method = "pairwise",
      by = "rater_type",
      adjust = "fdr"
    ),

  contrast_emm_sexes_pgs_2sd = 
    contrast_with_unadj(
      get_emm_rater_sex(two_way_model, at = list(PGS_scaled = 2)), # this is the contrast at 2SD above the mean of PGS
      method = "pairwise",
      by = "rater_type",
      adjust = "fdr"
    ),

  # pairwise contrasts between rater types within each sex
  contrast_emm_raters =
    contrast_with_unadj(
      get_emm_rater_sex(two_way_model),
      method = "pairwise",
      by = "sex",
      adjust = "fdr"
    ),

  # cumulative probabilities for each autism score level (no, low, high) within each rater type and sex
  estimated_prob_rater_sex =
    as.data.frame(get_eprob_rater_sex(two_way_model, outcome_var)),

  # pairwise contrasts between rater types within each sex for the estimated probabilities
  contrast_eprob_sexes =
    contrast_with_unadj(
      get_eprob_rater_sex(two_way_model, outcome_var),
      method = "pairwise",
      by = c("rater_type", outcome_var),
      adjust = "fdr"
    ),
  
  contrast_eprob_raters =
    contrast_with_unadj(
      get_eprob_rater_sex(two_way_model, outcome_var),
      method = "pairwise",
      by = c("sex", outcome_var),
      adjust = "fdr"
    ),

  # pgs association with latent autism score ordinal within each rater type and sex
  three_way_emtrends =
    as.data.frame(get_slopes_three_way(three_way_model)),

  # pairwise contrast between sexes of pgs association with probability of autism score ordinal within each rater type
  three_way_contrast_sexes =
    contrast_with_unadj(
      get_slopes_three_way(three_way_model),
      method = "pairwise",
      by = "rater_type",
      adjust = "fdr"
    ),

  # pairwise contrast between rater types of pgs association with probability of autism score ordinal within
  three_way_contrast_raters =
    contrast_with_unadj(
      get_slopes_three_way(three_way_model),
      method = "pairwise",
      by = "sex",
      adjust = "fdr"
    )
)}


# We were interested in whether ratings were influenced by congruency between the sex of the rater and the sex of the participant.
# Here we calculate average of same-sex dyads minus average of cross-sex dyads
get_parent_child_dyad = function(model_fit) {
  emm_parent_subset <- emmeans(
    model_fit, 
    ~ rater_type * sex, 
    at = list(rater_type = c("m12","v12")),
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

contrast_sex_dyads = contrast_with_unadj(get_parent_child_dyad(fit_m3), method = list("same_vs_cross" = c(-1, 1, 1, -1))) # p value for whether incongruency effect exists
contrast_sex_dyads_sensitivity = contrast_with_unadj(get_parent_child_dyad(fit_m3_sensitivity), method = list("same_vs_cross" = c(-1, 1, 1, -1))) 
contrast_sex_dyads_sensitivity_all_raters = contrast_with_unadj(get_parent_child_dyad(fit_m3_sensitivity_all_raters), method = list("same_vs_cross" = c(-1, 1, 1, -1))) 

# append contrast_sex_dyads in one table
all_contrast_sex_dyads = rbind(
  contrast_sex_dyads %>% mutate(dataset = "main"),
  contrast_sex_dyads_sensitivity %>% mutate(dataset = "sensitivity"),
  contrast_sex_dyads_sensitivity_all_raters %>% mutate(dataset = "sensitivity_all_raters")
) %>%
  select(dataset, everything()) # move dataset column to the front

# remove df columns which are not meaningful for clmm models
all_contrast_sex_dyads = select(all_contrast_sex_dyads, -contains("df"))
post <- drop_df_columns(post)
post_sensitivity <- drop_df_columns(post_sensitivity)
post_sensitivity_all_raters <- drop_df_columns(post_sensitivity_all_raters)

# save results
dir.create("results/post_hoc_investigations", showWarnings = FALSE, recursive = TRUE) # create directory if it doesn't exist
openxlsx::write.xlsx(all_contrast_sex_dyads, "results/post_hoc_investigations/contrast_sex_dyads.xlsx") # save as xlsx
openxlsx::write.xlsx(post, "results/post_hoc_investigations/post.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity, "results/post_hoc_investigations/post_sensitivity.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity_all_raters, "results/post_hoc_investigations/post_sensitivity_all_raters.xlsx") # save as xlsx



