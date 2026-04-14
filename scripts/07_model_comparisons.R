# This script performs model comparisons for the ordinal regression models fitted in the previous script (06_modeling.R)
# Determines the best fitting model(s) based on AIC and likelihood ratio tests, and checks model assumptions

rm(list = ls(all = TRUE))
gc()

library(ordinal)
library(car)
library(performance)

fit_m1 <- readRDS("results/models/fit_m1_clmm.rds")
fit_m2 <- readRDS("results/models/fit_m2_clmm.rds")
fit_m3 <- readRDS("results/models/fit_m3_clmm.rds")
fit_m4 <- readRDS("results/models/fit_m4_clmm.rds")
fit_m5 <- readRDS("results/models/fit_m5_clmm.rds")

# uncomment these to see results of sensitivity analysis models
# fit_m1 <- readRDS("results/models/sensitivity/fit_m1_clmm_sensitivity.rds")
# fit_m2 <- readRDS("results/models/sensitivity/fit_m2_clmm_sensitivity.rds")
# fit_m3 <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
# fit_m4 <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds")
# fit_m5 <- readRDS("results/models/sensitivity/fit_m5_clmm_sensitivity.rds")


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
table(mf$sex, mf$rater_type)
table(mf$sex, mf$rater_type, mf$autism_score_ordinal)

# collinearity checks
check_collinearity(fit_m2) # without interaction terms as VIF will be unreliable because of these
# results look fine
check_collinearity(fit_m3)
check_collinearity(fit_m4)

car::vif(fit_m2) # results with this package look fine too
car::vif(fit_m3)
car::vif(fit_m4)

# chisq of each predictor in best fitting models
Anova(fit_m3) 
Anova(fit_m4)


correlations_predictors = cov2cor(vcov(fit_m3))

library(openxlsx)
openxlsx::write.xlsx(correlations_predictors, "results/correlations_predictors_m3.xlsx")
