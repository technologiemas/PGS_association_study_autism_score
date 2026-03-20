# This script calculates descriptive statistics and performs statistical tests
# The statistics are not the correct way to do this actually so they are left out in the manuscript

rm(list = ls(all = TRUE))
gc()

library(dplyr)
library(moments)
library(tidyr)
library(psych)
library(openxlsx)

data = readRDS("data/processed/02_full_dataset_clean.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")


# --- distribution of age in the sample ---

data_mother = data_long %>%
filter(rater_type == "Mother")

data_self = data_long %>%
filter(rater_type == "Self")

hist(data_mother$age)
hist(data_self$age, breaks = 30)
table(data_mother$age)
table(data_self$age)


# --- distribution of phenotype and PGS data ---

shapiro.test(sample(data_long$autism_score[data_long$`rater_type` == "Mother"], 2000)) # result: not normally distributed
shapiro.test(sample(data_long$autism_score[data_long$`rater_type` == "Father"], 2000)) # result: not normally distributed
shapiro.test(sample(data_long$autism_score[data_long$`rater_type` == "Teacher"], 2000)) # result: not normally distributed
shapiro.test(sample(data_long$autism_score[data_long$`rater_type` == "Self"], 2000)) # result: not normally distributed

shapiro.test(sample(data$P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1, 2000)) # result: normally distributed


# --- descriptives and pairwise statistical tests ---

calculate_descriptives_phenotype <- function(data_long, phenotype) {
  # Calculate descriptive statistics and perform statistical tests for the phenotype autism scores per rater
  #
  # Args:
  #   data_long: A data frame containing the data in long format
  #   phenotype: The rater for which to calculate descriptives and perform tests.
  #
  # Returns:
  #   A data frame with descriptive statistics and p-values
  
  # one t-test per rater_type and correct for multiple testing
  pvals <- data_long %>%
    group_by(`rater_type`) %>%
    summarise(p_value = wilcox.test({{phenotype}} ~ `sex`)$p.value, # this test was determined as data was non normally distributed according to the Shapiro-Wilk test (above)
              .groups = "drop")

  # calculate effect sizes
  effect_sizes <- data_long %>%
    group_by(`rater_type`) %>%
    reframe(
      cohen_d = cohen.d({{phenotype}} ~ `sex`, data = cur_data())$cohen.d[2],
    )

  # calculate descriptives for each rater type and sex
  descriptives <- data_long %>%
    group_by(`rater_type`, `sex`) %>%
    summarise(
      n         = sum(!is.na({{phenotype}})),
      `age (mean)`       = mean(age, na.rm = TRUE),
      `age (sd)`         = sd(age, na.rm = TRUE),
      `autism score (mean)`      = mean({{phenotype}}, na.rm = TRUE),
      sd        = sd({{phenotype}},   na.rm = TRUE),
      skewness  = skewness({{phenotype}},  na.rm = TRUE),
      kurtosis  = kurtosis({{phenotype}},  na.rm = TRUE),
      .groups   = "drop"
    ) 
    # %>%
  #   left_join(pvals, by = "rater_type") %>%
  #   left_join(effect_sizes, by = "rater_type")

  # descriptives$p_value = descriptives$p_value * 4 # Bonferroni correction for 4 tests

  return(descriptives)
}

calculate_descriptives_genotype <- function(data, PGS) {
  # Calculate descriptive statistics and perform statistical tests for the polygenic scores
  #
  # Args:
  #   data: A data frame containing the data where each row is a FISNumber (participant)
  #   PGS: The polygenic score variable
  #
  # Returns:
  #   A data frame with descriptive statistics and p-values

  # filter on unique FISNumber to avoid duplicates in the data (as the data is in long format with multiple rows per participant)
  data_unique = data %>%
    group_by(FISNumber) %>%
    slice(1) %>%
    ungroup() 

  # one t-test per rater_type
  pvals <- data_unique %>%
    summarise(t_test_p = t.test({{PGS}} ~ `sex`)$p.value, # t-test was chosen as data is normally distributed according to Shapiro-Wilk test (above)
              .groups = "drop")

  # calculate effect sizes
  effect_sizes <- data_unique %>%
    reframe(
      cohen_d = cohen.d({{PGS}} ~ `sex`, data = cur_data())$cohen.d[2],
    )

  pgs_str = sub(".*_LDp1", "", deparse(substitute(PGS)))

  # calculate descriptives for each rater type and sex
  descriptives <- data_unique %>%
    group_by(`sex`) %>%
    summarise(
      name = paste0("PGS", pgs_str),
      n         = sum(!is.na({{PGS}})),
      `PGS (mean)`      = mean({{PGS}}, na.rm = TRUE),
      sd        = sd({{PGS}},   na.rm = TRUE),
      skewness  = skewness({{PGS}},  na.rm = TRUE),
      kurtosis  = kurtosis({{PGS}},  na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(p_value = pvals$t_test_p,
           cohen_d = effect_sizes$cohen_d)
  
  return(descriptives)
}

# correlation matrix of all raters separated on sex
data_wide = data %>%
  pivot_wider(names_from = sex, values_from = c(m12_aut_sum, v12_aut_sum, t12_aut_sum, ysr14_aut_sum))

cor_matrix_females = cor(select(data_wide, m12_aut_sum_Female, v12_aut_sum_Female, t12_aut_sum_Female, ysr14_aut_sum_Female), use = "pairwise.complete.obs")
cor_matrix_males = cor(select(data_wide, m12_aut_sum_Male, v12_aut_sum_Male, t12_aut_sum_Male, ysr14_aut_sum_Male), use = "pairwise.complete.obs")
cor_all = cor(select(data, m12_aut_sum, v12_aut_sum, t12_aut_sum, ysr14_aut_sum), use = "pairwise.complete.obs")

# count the number of individuals for which each rater type is available for that individual
data_overlap = data %>%
  filter(!is.na(m12_aut_sum) & !is.na(v12_aut_sum) & !is.na(t12_aut_sum) & !is.na(ysr14_aut_sum))

data_overlap_no_father = data %>%
  filter(!is.na(m12_aut_sum) & !is.na(t12_aut_sum) & !is.na(ysr14_aut_sum))

# --- show / save results ---

descriptives_phenotype <- calculate_descriptives_phenotype(data_long, `autism_score`)
descriptives_genotype <- calculate_descriptives_genotype(data, `P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1`)
data$P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1_scaled <- scale(data$P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1)
descriptives_genotype_scaled <- calculate_descriptives_genotype(data, `P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1_scaled`)

# descriptives_phenotype
# descriptives_genotype

# cor_matrix_females
# cor_matrix_males

# nrow(data_overlap[data_overlap$sex == "Male", ]) # overlap sample males: 414
# nrow(data_overlap[data_overlap$sex == "Female", ]) # overlap sample females: 624

# nrow(data_overlap_no_father[data_overlap_no_father$sex == "Male", ]) # overlap sample: 490
# nrow(data_overlap_no_father[data_overlap_no_father$sex == "Female", ]) # overlap sample: 753

# append descriptives with data_overlap sample size
descriptives_phenotype = descriptives_phenotype %>%
  bind_rows(
    data.frame(
      rater_type = "overlap all raters",
      sex = c("Male", "Female"),
      n = c(nrow(data_overlap[data_overlap$sex == "Male", ]), nrow(data_overlap[data_overlap$sex == "Female", ]))))


# --- collect results ---

lst_correlations = list(
  cor_all = cor_all,
  cor_matrix_females = cor_matrix_females,
  cor_matrix_males   = cor_matrix_males
)
# append descriptives_genotype_scaled to descriptives_genotype
descriptives_genotype = descriptives_genotype %>%
  bind_rows(descriptives_genotype_scaled)

lst_descriptives = list(
  descriptives_phenotype = descriptives_phenotype,
  descriptives_genotype = descriptives_genotype
)

# save files
openxlsx::write.xlsx(lst_correlations, "results/correlation_matrices.xlsx", rowNames = FALSE)
openxlsx::write.xlsx(lst_descriptives, "results/descriptives.xlsx", rowNames = FALSE)