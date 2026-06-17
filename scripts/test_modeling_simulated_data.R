# This script does simulated data modeling to test model formulas for the main analyses
# The ordinal model seems like the best fit, as the linear model violates assumptions of normality of residuals

rm(list = ls(all = TRUE))
gc()

# step 0: baseline variance partition - random intercepts only
m0 = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber)"

# step 1: main effects
m1 = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber) +
    PGS + sex + rater"

# step 2: main + covariates
m2 = "autism_score ~ PGS + sex + rater + (1 | FamilyNumber) + (1 | FISNumber) + 
      PLATFORM + age +
      PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG"

# step 3: the three two-way interactions
m3 = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS * rater + PGS * sex + sex * rater + 
      PLATFORM + age +
      PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG"

# step 3b: two-way interactions with covariate interactions following Keller, 2014
# m3b = "autism_score ~ + (1 | FamilyNumber) + (1 | FISNumber) +
#       PGS * rater + PGS * sex + sex * rater +
#       (PLATFORM + age + PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG) * (PGS + sex + rater)
#       "

# step 4: three-way interaction
m4 = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS * rater * sex +
      PLATFORM + age +
      PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG"

# step 4b: three-way interaction with covariate interactions following Keller, 2014
m4b = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS * rater * sex +
      (PLATFORM + age + PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG) * (PGS + sex + rater)"


# --- test the model formulas with simulated data to ensure they contain the correct terms ---
library(lme4)
set.seed(4)
n = 100
df <- data.frame(
  autism_score = rnorm(n),
  FamilyNumber = factor(sample(1:20, n, replace = TRUE)),
  # FINumber ranging from 1 to n
  FISNumber    = factor(sample(1:50, n, replace = TRUE)),
  PGS_1          = rnorm(n),
  PGS_2          = rnorm(n, mean = 0.5), 
  rater   = factor(sample(c("father", "teacher", "mother", "self"), n, replace = TRUE)),
  sex          = factor(sample(c("M", "F"), n, replace = TRUE)),
  PLATFORM     = factor(sample(c("illumina", "gplatform2"), n, replace = TRUE)),
  age          = rnorm(n, 10, 2),
  PC1_1KG      = rnorm(n),
  PC2_1KG      = rnorm(n),
  PC3_1KG      = rnorm(n),
  PC4_1KG      = rnorm(n),
  PC5_1KG      = rnorm(n),
  PC6_1KG      = rnorm(n),
  PC7_1KG      = rnorm(n),
  PC8_1KG      = rnorm(n),
  PC9_1KG      = rnorm(n),
  PC10_1KG     = rnorm(n)
)

fit_m0  <- lmer(as.formula(m0),   data = df)
fit_m1 <- lmer(as.formula(m1),   data = df)
fit_m2  <- lmer(as.formula(m2),   data = df)
fit_m3   <- lmer(as.formula(m3),   data = df)
fit_m3b   <- lmer(as.formula(m3b),   data = df)
fit_m4   <- lmer(as.formula(m4),   data = df)
fit_m4b   <- lmer(as.formula(m4b),   data = df)

# extract all effects of model fit_m4b
effects_m0 <- fixef(fit_m0)
summary(fit_m0)

effects_m1 <- fixef(fit_m1)
summary(fit_m1)

effects_m2 <- fixef(fit_m2)
summary(fit_m2)

effects_m3 <- fixef(fit_m3)
summary(fit_m3)

effects_m3b <- fixef(fit_m3b)
summary(fit_m3b)

effects_m3c <- fixef(fit_m3c)
summary(fit_m3c)

effects_m4 <- fixef(fit_m4)
summary(fit_m4)

effects_m4b <- fixef(fit_m4b)
summary(fit_m4b)

# --- ordinal model with clmm() from ordinal package ---
library(ordinal)

# make ordinal outcome variable (simulated data)
df = df %>%
  mutate(autism_score_ordinal = cut(autism_score,
                                 breaks = c(-Inf, -1, 1, Inf),
                                 labels = c("no", "mild", "high"),
                                 right = FALSE))

m3ord = "autism_score_ordinal ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS * rater + PGS * sex + sex * rater + 
      PLATFORM + age +
      PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG"


fit_m3ord <- clmm(as.formula(m3ord),
  data = df,
  link = "logit",
  threshold = "flexible" # something with thresholds maybe needing to be the same across sexes
)

summary(fit_m3ord)


# Results conclude residuals are not normally distributed, so we will proceed with ordinal models





# testing for Hayley

library(geepack)
library(cocor)

f1 = "autism_score ~ PGS_1 + sex + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"
f2 = "autism_score ~ PGS_2 + sex + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"

covs <- "~ sex + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG" # defining covariates

m_aut <- geeglm(as.formula("autism_score ~ sex + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"), data = df, id = FamilyNumber, corstr = "exchangeable")
res_autism <- residuals(m_aut)

m_pgs <- geeglm(as.formula("PGS_1 ~ sex + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"), data = df, id = FamilyNumber, corstr = "exchangeable")
res_pgs <- residuals(m_pgs)

m_age <- geeglm(as.formula("PGS_2 ~ sex + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"), data = df, id = FamilyNumber, corstr = "exchangeable")
res_age <- residuals(m_age)

df_res <- data.frame(
  autism_score = as.numeric(res_autism),
  PGS_1    = as.numeric(res_pgs),
  PGS_2    = as.numeric(res_age)
)

cocor(~autism_score + PGS_1 | autism_score + PGS_2, data = df_res)

# this basically looks at the correlations between autism_score and PGS_1, autism_score and PGS_2, and PGS_1 and PGS_2. Tests of the difference between these two correlations is significant, accounting for the fact that both correlations share the same variable (autism_score) and are thus not independent.
# and: cocor(~adhd_ia + PGS_1 | adhd_ha + PGS_1, data = df_res),  etc
# cocor(~adhd_ia + PGS_1 | adhd_ia + PGS_2, data = df_res)
# cocor(~adhd_ha + PGS_1 | adhd_ha + PGS_2, data = df_res)
# cocor(~adhd_ia + PGS_1 | adhd_ha + PGS_1, data = df_res)


# ALTERNATIVELY: using prevalence or multcomp to do a "horserace" analysis of PGS_1 vs PGS_2 in the same model, to see if one is significantly stronger than the other in predicting autism_score
library(multcomp)

data <- df
data$PGS_1_z <- as.numeric(scale(data$PGS_1))
data$PGS_2_z <- as.numeric(scale(data$PGS_2))

horse_race_model <- geeglm(
      autism_score ~ PGS_1_z + PGS_2_z + sex + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG,
      data = data,
      id = FamilyNumber,
      corstr = "exchangeable"
)

# Compare the standardized effects directly:
# if the difference is significantly different from 0, the larger coefficient
# indicates the stronger predictor per 1 SD increase in PGS.
horse_race_test <- glht(
      horse_race_model,
      linfct = c("PGS_1_z - PGS_2_z = 0")
)

summary(horse_race_model)
summary(horse_race_test)

# This is probably easier to conceptialize. The problem with this is that you also want to test if PGS_1 predicts ADHD_IA or ADHD_HA better


library(geepack)
# For choosing the pgs threshold, simply look which has the best predictive value:
pgs1_model <- geeglm(as.formula("autism_score ~ PGS_threshold_1 + sex + age + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"), data = df_clean, id = FamilyNumber, corstr = "exchangeable")
pgs2_model <- geeglm(as.formula("autism_score ~ PGS_threshold_2 + sex + age + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"), data = df_clean, id = FamilyNumber, corstr = "exchangeable")
pgs3_model <- geeglm(as.formula("autism_score ~ PGS_threshold_3 + sex + age + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"), data = df_clean, id = FamilyNumber, corstr = "exchangeable")

# gee: "autism_score ~ PGS_threshold_1 + sex + age + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"
# clmm: "autism_score ~ (1 | FamilyNumber) + PGS_threshold_1 + sex + age + rater + PLATFORM + PC1_1KG + PC2_1KG + PC3_1KG"