# This script runs the models for the main modeling analyses using ordinal regression with clmm from the ordinal package
# Also has some bayesian modeling
# Takes a long time to run be warned! 

rm(list = ls(all = TRUE))
gc()

# --- LIBRARIES ---
library(dplyr)
library(lme4)
library(lmerTest) # This adds p-values to lmer
library(emmeans)
library(ggplot2)


source("scripts/_helper_functions.R")


# --- Function for running the models ---

run_ordinal_clmm = function (formula_str, data) {
  library(ordinal)
  fit <- clmm(as.formula(formula_str),
              data = data,
              link = "logit",
              threshold = "flexible")
  return(fit)
}

# --- LOAD DATA ---
data_long = readRDS("data/processed/02_full_dataset_long.rds")

# --- PRE-PROCESSING & FORMATTING ---

# Scaling Continuous Variables
# We scale PCs, PGS, and both versions of the continuous autism score (for linear models)
vars_to_scale <- c("autism_score", "autism_score_sensitivity", "age", "date_of_assessment") # TODO: , "date_of_assessment_numeric"
data_long = data_long %>%
  mutate(across(all_of(vars_to_scale),
                ~ as.numeric(scale(.)), .names = "{col}_scaled")) 


# subset to teacher data with monozygotic twins and where two teachers with different genders rated the twins
teacher_data <- data_long %>%
  # Filter for Teacher ratings and MZ twins
  filter(rater_type == "Teacher", 
         twzyg %in% c(1, 3)) %>%
  # Label the sex for clarity
  mutate(twin_sex = ifelse(twzyg == 1, "Male Twin", "Female Twin")) %>%
  # Group by family to check the teacher-sex condition
  group_by(FamilyNumber) %>%
  # Keep only families where there are 2 ratings and 2 different teacher sexes
  filter(n() == 2) %>%
  filter(any(genderlkrt12 == "Male") & any(genderlkrt12 == "Female")) %>%
  ungroup()

# The same thing but for mothers and fathers 
parent_data <- data_long %>%
  # Filter for mother and father ratings and MZ twins
  filter(rater_type %in% c("Mother", "Father"),
         twzyg %in% c(1, 3)) %>%
  # Label the sex for clarity
  mutate(twin_sex = ifelse(twzyg == 1, "Male Twin", "Female Twin")) %>%
  group_by(FamilyNumber) %>%
  # Keep only families where there are 2 ratings and 2 different parents sexes
  filter(n() == 2) %>%
  filter(any(rater_type == "Mother") & any(rater_type == "Father")) %>%
  ungroup()

# Ensure your sex variables are factors for the interaction
teacher_data$twin_sex <- as.factor(teacher_data$twin_sex)
parent_data$twin_sex <- as.factor(parent_data$twin_sex)

# Checking data types
sapply(teacher_data, class)
class(teacher_data$autism_score_ordinal) 

# --- MODEL FORMULA DEFINITIONS ---

# looking at the interaction between teacher and twin sex
m1_main = "autism_score_scaled ~ twin_sex * genderlkrt12 + (1 | FamilyNumber) + age_scaled"
m1_main_ordinal = "autism_score_ordinal ~ twin_sex * genderlkrt12 + (1 | FamilyNumber) + age_scaled"

# test using linear modeling

fit_m1_linear = lmer(m1_main, data = teacher_data)
fit_m1_ordinal = run_ordinal_clmm(m1_main_ordinal, teacher_data)
summary(fit_m1_ordinal)

emmeans(fit_m1_ordinal, ~ twin_sex * genderlkrt12)
contrast(emmeans(fit_m1_ordinal, ~ twin_sex * genderlkrt12), method = "pairwise") # looking at the pairwise comparisons

plot_teacher_twins = ggplot(teacher_data, aes(x = genderlkrt12, y = autism_score_scaled, color = twin_sex, group = twin_sex)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(title = "Interaction: Teacher Sex vs Twin Sex",
       x = "Teacher Sex",
       y = "Mean Autism Score",
       color = "Twin Sex") +
  theme_minimal() +
    ylim(-2, 2) # Adjust y-axis limits for better visualization

ggsave(
  "results/figures/teacher_twins.png",
  plot_teacher_twins,
  device = "png",
  width = 8.4, height = 6, units = "cm",
  dpi = 600, scale = 2
)

# --- same as above but now for mothers and fathers ---

m2_main = "autism_score_scaled ~ twin_sex * rater_type + (1 | FamilyNumber) + age_scaled" # rater_type indicates mother and father
m2_main_ordinal = "autism_score_ordinal ~ twin_sex * rater_type + (1 | FamilyNumber) + age_scaled"

fit_m2_linear = lmer(m2_main, data = parent_data)
fit_m2_ordinal = run_ordinal_clmm(m2_main_ordinal, parent_data)
summary(fit_m2_ordinal)

emmeans(fit_m2_ordinal, ~ twin_sex * rater_type)
contrast(emmeans(fit_m2_ordinal, ~ twin_sex * rater_type), method = "pairwise") # looking at the pairwise comparisons

plot_parent_twins = ggplot(parent_data, aes(x = rater_type, y = autism_score_scaled, color = twin_sex, group = twin_sex)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(title = "Interaction: Parent Sex vs Twin Sex",
       x = "Parent Sex",
       y = "Mean Autism Score",
       color = "Twin Sex") +
  ylim(-2, 2) # Adjust y-axis limits for better visualization

ggsave(
  "results/figures/parent_twins.png",
  plot_parent_twins,
  device = "png",
  width = 8.4, height = 6, units = "cm",
  dpi = 600, scale = 2
)

# ------------------------------------



# subset to teacher data with both a male and female rater (teacher) present
  # Filter for Teacher ratings and MZ twins
  filter(rater_type == "Teacher") %>%
  # Group by family to check the teacher-sex condition
  group_by(FamilyNumber) %>%
  filter(any(genderlkrt12 == "Male") & any(genderlkrt12 == "Female")) %>%
  ungroup()

# The same thing but for mothers and fathers, where both are present
parent_data_all <- data_long %>%
  # Filter for mother and father ratings and MZ twins
  filter(rater_type %in% c("Mother", "Father")) %>%
  # Label the sex for clarity
  group_by(FamilyNumber) %>%
  # Keep only families where there are 2 ratings and 2 different parents sexes
  filter(n() == 2) %>%
  filter(any(rater_type == "Mother") & any(rater_type == "Father")) %>%
  ungroup()


# --- MODEL FORMULA DEFINITIONS ---

# looking at the interaction between teacher and twin sex
m1_main = "autism_score_scaled ~ sex * genderlkrt12 + (1 | FamilyNumber) + age_scaled"
m1_main_ordinal = "autism_score_ordinal ~ sex * genderlkrt12 + (1 | FamilyNumber) + age_scaled"

# test using linear modeling

fit_m1_linear = lmer(m1_main, data = teacher_data_all)
fit_m1 = run_ordinal_clmm(m1_main_ordinal, teacher_data_all)
summary(fit_m1)

emmeans(fit_m1, ~ sex * genderlkrt12)
contrast(emmeans(fit_m1, ~ sex * genderlkrt12), method = "pairwise") # looking at the pairwise comparisons

ggplot(teacher_data_all, aes(x = genderlkrt12, y = autism_score_scaled, color = sex, group = sex)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(title = "Interaction: Teacher Sex vs Child Sex",
       x = "Teacher Sex",
       y = "Mean Autism Score",
       color = "Twin Sex") +
  theme_minimal() +
    ylim(-2, 2) # Adjust y-axis limits for better visualization


# --- same as above but now for mothers and fathers ---

m2_main = "autism_score_scaled ~ sex * rater_type + (1 | FamilyNumber) + age_scaled" # rater_type indicates mother and father
m2_main_ordinal = "autism_score_ordinal ~ sex * rater_type + (1 | FamilyNumber) + age_scaled"

fit_m2_linear = lmer(m2_main, data = parent_data_all)
fit_m2_ordinal = run_ordinal_clmm(m2_main_ordinal, parent_data_all)
summary(fit_m2_ordinal)

emmeans(fit_m2_ordinal, ~ sex * rater_type)
contrast(emmeans(fit_m2_ordinal, ~ sex * rater_type), method = "pairwise") # looking at the pairwise comparisons
contrast(emmeans(fit_m2_ordinal, ~ sex * rater_type),  method = list("same_vs_cross" = c(-1, 1, 1, -1))) # looking at the pairwise comparisons

ggplot(parent_data_all, aes(x = rater_type, y = autism_score_scaled, color = sex)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  labs(title = "Interaction: Parent Sex vs Child Sex",
       x = "Parent Sex",
       y = "Mean Autism Score",
       color = "Child Sex") +
  theme_minimal() + 
  ylim(-2, 2) # Adjust y-axis limits for better visualization
