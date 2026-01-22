rm(list = ls(all = TRUE))
gc()

# library(ggplot2)
library(dplyr)
library(moments)
# library(bestNormalize)
# library(pscl)
library(tidyr)
# library(ggpubr)
library(psych)

data = readRDS("data/processed/02_full_dataset_clean.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")


# --- distribution of phenotype and PGS data ---

shapiro.test(sample(data_long$autism_score[data_long$`rater_type` == "m12"], 2000)) # result: not normally distributed
shapiro.test(sample(data_long$autism_score[data_long$`rater_type` == "v12"], 2000)) # result: not normally distributed
shapiro.test(sample(data_long$autism_score[data_long$`rater_type` == "t12"], 2000)) # result: not normally distributed
shapiro.test(sample(data_long$autism_score[data_long$`rater_type` == "ysr14"], 2000)) # result: not normally distributed

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
      mean      = mean({{phenotype}}, na.rm = TRUE),
      sd        = sd({{phenotype}},   na.rm = TRUE),
      skewness  = skewness({{phenotype}},  na.rm = TRUE),
      kurtosis  = kurtosis({{phenotype}},  na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    left_join(pvals, by = "rater_type") %>%
    left_join(effect_sizes, by = "rater_type")

  descriptives$p_value = descriptives$p_value * 4 # Bonferroni correction for 4 tests

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

  # one t-test per rater_type
  pvals <- data %>%
    summarise(t_test_p = t.test({{PGS}} ~ `sex`)$p.value, # t-test was chosen as data is normally distributed according to Shapiro-Wilk test (above)
              .groups = "drop")

  # calculate effect sizes
  effect_sizes <- data %>%
    reframe(
      cohen_d = cohen.d({{PGS}} ~ `sex`, data = cur_data())$cohen.d[2],
    )

  # calculate descriptives for each rater type and sex
  descriptives <- data %>%
    group_by(`sex`) %>%
    summarise(
      n         = sum(!is.na({{PGS}})),
      mean      = mean({{PGS}}, na.rm = TRUE),
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

cor_matrix_females = cor(select(data_wide, m12_aut_sum_FEMALE, v12_aut_sum_FEMALE, t12_aut_sum_FEMALE, ysr14_aut_sum_FEMALE), use = "pairwise.complete.obs")
cor_matrix_males = cor(select(data_wide, m12_aut_sum_MALE, v12_aut_sum_MALE, t12_aut_sum_MALE, ysr14_aut_sum_MALE), use = "pairwise.complete.obs")

# count the number of individuals for which each rater type is available for that individual
data_overlap = data %>%
  filter(!is.na(m12_aut_sum) & !is.na(v12_aut_sum) & !is.na(t12_aut_sum) & !is.na(ysr14_aut_sum))

data_overlap_no_father = data %>%
  filter(!is.na(m12_aut_sum) & !is.na(t12_aut_sum) & !is.na(ysr14_aut_sum))

# --- show / save results ---

descriptives_phenotype <- calculate_descriptives_phenotype(data_long, `autism_score`)
descriptives_genotype <- calculate_descriptives_genotype(data, `P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1`)

descriptives_phenotype
descriptives_genotype

cor_matrix_females
cor_matrix_males

nrow(data_overlap[data_overlap$sex == "MALE", ]) # overlap sample males: 414
nrow(data_overlap[data_overlap$sex == "FEMALE", ]) # overlap sample females: 624

nrow(data_overlap_no_father[data_overlap_no_father$sex == "MALE", ]) # overlap sample: 490
nrow(data_overlap_no_father[data_overlap_no_father$sex == "FEMALE", ]) # overlap sample: 753

# append descriptives with data_overlap
descriptives_phenotype = descriptives_phenotype %>%
  bind_rows(
    data.frame(
      rater_type = "overlap all raters",
      sex = c("MALE", "FEMALE"),
      n = c(nrow(data_overlap[data_overlap$sex == "MALE", ]), nrow(data_overlap[data_overlap$sex == "FEMALE", ]))))


# save descriptives
write.csv(descriptives_phenotype, "results/descriptives_phenotype.csv", row.names = FALSE)
write.csv(descriptives_genotype, "results/descriptives_genotype.csv", row.names = FALSE)
write.csv(cor_matrix_females, "results/correlation_matrix_females.csv", row.names = TRUE)
write.csv(cor_matrix_males, "results/correlation_matrix_males.csv", row.names = TRUE)