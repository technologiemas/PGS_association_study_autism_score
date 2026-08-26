# This script performs model comparisons for the ordinal regression models fitted in the previous script (06_modeling.R)
# Determines the best fitting model(s) based on AIC and likelihood ratio tests, and checks model assumptions

rm(list = ls(all = TRUE))
gc()

library(ordinal)
library(performance)
library(emmeans)

fit_m1 <- readRDS("results/models/fit_m1_clmm.rds")
fit_m2 <- readRDS("results/models/fit_m2_clmm.rds")
fit_m3 <- readRDS("results/models/fit_m3_clmm.rds")
fit_m4 <- readRDS("results/models/fit_m4_clmm.rds")
fit_m5 <- readRDS("results/models/fit_m5_clmm.rds")

# uncomment these to see results of sensitivity analysis models
# fit_m3 <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
# fit_m4 <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds")

# --- MODEL SUMMARIES ---
# summary(fit_m1)
# summary(fit_m2)
# summary(fit_m3)
# summary(fit_m4)
# summary(fit_m5)

# --- MODEL COMPARISONS ---
# model comparisons can be done using AIC in combination with likelihood ratio tests
anova(fit_m1, fit_m2, fit_m3, fit_m4, fit_m5) # models 3 and 4 are best

# best models performance checks
performance(fit_m1)
performance(fit_m2)
performance(fit_m3)
performance(fit_m4)
performance(fit_m5)

# exploration of sample sizes
mf <- model.frame(fit_m4)
table(mf$sex, mf$rater)
table(mf$sex, mf$rater, mf$autism_score_ordinal)

# collinearity checks
check_collinearity(fit_m2)
check_collinearity(fit_m3) 
check_collinearity(fit_m4) # all look fine

# chisq and wald test of each predictor in best fitting models following emmeans joint_tests
joint_tests(fit_m3,
            nuisance = c("PC1_scaled",  "PC2_scaled",  "PC3_scaled",
                         "PC4_scaled",  "PC5_scaled",  "PC6_scaled",
                         "PC7_scaled",  "PC8_scaled",  "PC9_scaled",
                         "PC10_scaled",
                         "PLATFORM"))

joint_tests(fit_m4,
            nuisance = c("PC1_scaled",  "PC2_scaled",  "PC3_scaled",
                         "PC4_scaled",  "PC5_scaled",  "PC6_scaled",
                         "PC7_scaled",  "PC8_scaled",  "PC9_scaled",
                         "PC10_scaled",
                         "PLATFORM"))

correlations_predictors = cov2cor(vcov(fit_m3)) # no very high correlations

library(openxlsx)
openxlsx::write.xlsx(correlations_predictors, "results/correlations_predictors_m3.xlsx")





# TODO: delete?? Or do I want the r2 for PGS after all
fit_pgs <- clmm(as.formula(paste("autism_score_ordinal ~ ", m1_formula, "+ PGS_scaled")),
              data = data_long,
              link = "logit",
              threshold = "flexible")

# calculating nagelkerke's r2 for the PGS model vs the base model (only covariates/random effects)
ll_full = logLik(fit_pgs) # the base + pgs model. as.numeric() needed?
ll_null = logLik(fit_m1) # the base model
n = nobs(fit_pgs)
r2_2 = (1 - exp((2/n) * ((ll_null) - (ll_full)))) / (1 - exp((2/n) * (ll_null)))
r2_2

anova(fit_m1, fit_pgs)
