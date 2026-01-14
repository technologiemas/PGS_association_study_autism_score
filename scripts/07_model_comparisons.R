rm(list = ls(all = TRUE))
gc()

library(ordinal)
library(car)

fit_m1 <- readRDS("results/models/fit_m1_clmm.rds")
fit_m2 <- readRDS("results/models/fit_m2_clmm.rds")
fit_m3 <- readRDS("results/models/fit_m3_clmm.rds")
fit_m4 <- readRDS("results/models/fit_m4_clmm.rds")
fit_m4b <- readRDS("results/models/fit_m4b_clmm.rds")
fit_m5 <- readRDS("results/models/fit_m5_clmm.rds")

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
performance(fit_m3)
performance(fit_m4)

# exploration of sample sizes
mf <- model.frame(fit_m4)
table(mf$sex, mf$rater_type)
table(mf$sex, mf$rater_type, mf$autism_score_ordinal)

# collinearity checks
check_collinearity(fit_m3)
check_collinearity(fit_m4)
check_collinearity(fit_m4b)
check_collinearity(fit_m5)

# chisq of each predictor in best fitting models
Anova(fit_m3) 
Anova(fit_m4)

