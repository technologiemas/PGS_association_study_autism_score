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

# helper function to get contrasts with unadjusted p-values
contrast_with_unadj <- function(emm_obj, ..., adjust = "fdr") {
  ctr <- contrast(emm_obj, ..., adjust = "none")

  unadj <- summary(ctr, adjust = "none")
  adj   <- summary(ctr, adjust = adjust)

  out <- as.data.frame(adj)
  out$p_unadjusted <- unadj$p.value

  names(out)[names(out) == "p.value"] <- paste0("p_", adjust)

  out
}

# --- Two way interaction effects post hoc investigations ---

# estimated marginal means for rater type within each sex
get_emm_rater_sex = function(fit_model) {
  emmeans(fit_model, ~ rater_type | sex,
                          at = list(PGS_scaled = 0),
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

# # within each sex and autism score, pairwise contrasts between rater types
# contrast(
#   eprob_rater_sex,
#   method = "pairwise",
#   by = c("sex", "autism_score_ordinal"),
#   adjust = "fdr"
# )

# --- Three way interaction effect post hoc investigation ---

# three way PGS * sex * rater_type: slopes of PGS predicting autism score ordinal within each rater type and sex
get_slopes_three_way <- function(fit_model) {
  emtrends(
    fit_model,
    ~ sex | rater_type,
    var = "PGS_scaled",
    mode = "latent"
  )
}

# collect results and save as excel files
lst_results = function(two_way_model, three_way_model, outcome_var) {
  list(
  estimated_marginal_mean_rater_sex = as.data.frame(get_emm_rater_sex(fit_m3)),

  contrast_emm_raters =
    contrast_with_unadj(
      get_emm_rater_sex(two_way_model),
      method = "pairwise",
      adjust = "fdr"
    ),

  estimated_probability_rater_sex =
    as.data.frame(get_eprob_rater_sex(two_way_model, outcome_var)),

  contrast_eprob_sexes =
    contrast_with_unadj(
      get_eprob_rater_sex(two_way_model, outcome_var),
      method = "pairwise",
      by = c("rater_type", outcome_var),
      adjust = "fdr"
    ),

  three_way_emtrends =
    as.data.frame(get_slopes_three_way(three_way_model)),

  three_way_contrast_sexes =
    contrast_with_unadj(
      get_slopes_three_way(three_way_model),
      method = "pairwise",
      by = "rater_type",
      adjust = "fdr"
    ),

  three_way_contrast_raters =
    contrast_with_unadj(
      get_slopes_three_way(three_way_model),
      method = "pairwise",
      by = "sex",
      adjust = "fdr"
    )
)}

post = lst_results(fit_m3, fit_m4, "autism_score_ordinal")
post_sensitivity = lst_results(fit_m3_sensitivity, fit_m4_sensitivity, "autism_score_ordinal_sensitivity")
post_sensitivity_all_raters = lst_results(fit_m3_sensitivity_all_raters, fit_m4_sensitivity_all_raters, "autism_score_ordinal_sensitivity")

dir.create("results/post_hoc_investigations", showWarnings = FALSE, recursive = TRUE) # create directory if it doesn't exist
openxlsx::write.xlsx(post, "results/post_hoc_investigations/post.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity, "results/post_hoc_investigations/post_sensitivity.xlsx") # save as xlsx
openxlsx::write.xlsx(post_sensitivity_all_raters, "results/post_hoc_investigations/post_sensitivity_all_raters.xlsx") # save as xlsx
