
# step 0: baseline variance partition - random intercepts only
m0 = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber)"

# step 1: main effects
m1 = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber) +
    PGS + sex + rater_type"

# step 2: main + covariates
m2 = "autism_score ~ PGS + sex + rater_type + (1 | FamilyNumber) + (1 | FISNumber) + 
      PLATFORM + age +
      PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG"

# step 3: the three two-way interactions
m3 = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS * rater_type + PGS * sex + sex * rater_type + 
      PLATFORM + age +
      PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG"

# step 3b: two-way interactions with covariate interactions following Keller, 2014
# m3b = "autism_score ~ + (1 | FamilyNumber) + (1 | FISNumber) +
#       PGS * rater_type + PGS * sex + sex * rater_type +
#       (PLATFORM + age + PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG) * (PGS + sex + rater_type)
#       "

# step 4: three-way interaction
m4 = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS * rater_type * sex +
      PLATFORM + age +
      PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG"

# step 4b: three-way interaction with covariate interactions following Keller, 2014
m4b = "autism_score ~ (1 | FamilyNumber) + (1 | FISNumber) + 
      PGS * rater_type * sex +
      (PLATFORM + age + PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG) * (PGS + sex + rater_type)"


# --- test the model formulas with simulated data to ensure they contain the correct terms ---
library(lme4)
set.seed(4)
n = 100
df <- data.frame(
  autism_score = rnorm(n),
  FamilyNumber = factor(sample(1:20, n, replace = TRUE)),
  # FINumber ranging from 1 to n
  FISNumber    = factor(sample(1:50, n, replace = TRUE)),
  PGS          = rnorm(n),
  rater_type   = factor(sample(c("father", "teacher", "mother", "self"), n, replace = TRUE)),
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
      PGS * rater_type + PGS * sex + sex * rater_type + 
      PLATFORM + age +
      PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG"


fit_m3ord <- clmm(as.formula(m3ord),
  data = df,
  link = "logit",
  threshold = "flexible" # something with thresholds maybe needing to be the same across sexes
)

summary(fit_m3ord)


# Results conclude residuals are not normally distributed, so we will proceed with ordinal models