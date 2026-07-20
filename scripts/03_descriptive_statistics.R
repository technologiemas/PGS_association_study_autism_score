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

data_long_ysr12 = readRDS("data/processed/02_full_dataset_long_self_age_12.rds")

# --- distribution of age and date of assessment in the sample ---

data_mother = data_long %>%
filter(rater == "Mother")

data_self = data_long %>%
filter(rater == "Self")

hist(data_mother$age)
hist(data_self$age, breaks = 30)
table(data_mother$age)
table(data_self$age)

hist(data_mother$date_of_assessment)
hist(data_self$date_of_assessment)


# --- distribution of phenotype and PGS data ---

shapiro.test(sample(data_long$autism_score[data_long$`rater` == "Mother"], 2000)) # result: not normally distributed
shapiro.test(sample(data_long$autism_score[data_long$`rater` == "Father"], 2000)) # result: not normally distributed
shapiro.test(sample(data_long$autism_score[data_long$`rater` == "Teacher"], 2000)) # result: not normally distributed
shapiro.test(sample(data_long$autism_score[data_long$`rater` == "Self"], 2000)) # result: not normally distributed

shapiro.test(sample(data_long$PGS, 2000)) # result: normally distributed


# --- descriptives and pairwise statistical tests ---

calculate_descriptives_phenotype <- function(data_long, phenotype) {
  # one t-test per rater and correct for multiple testing
  pvals <- data_long %>%
    group_by(`rater`) %>%
    summarise(p_value = wilcox.test({{phenotype}} ~ `sex`)$p.value, # this test was determined as data was non normally distributed according to the Shapiro-Wilk test (above)
              .groups = "drop")

  # calculate effect sizes
  effect_sizes <- data_long %>%
    group_by(`rater`) %>%
    reframe(
      cohen_d = cohen.d({{phenotype}} ~ `sex`, data = cur_data())$cohen.d[2],
    )

  # calculate descriptives for each rater type and sex
  descriptives <- data_long %>%
    group_by(`rater`, `sex`) %>%
    summarise(
      n         = sum(!is.na({{phenotype}})),
      `age (mean)`       = mean(age, na.rm = TRUE),
      `age (sd)`         = sd(age, na.rm = TRUE),
      `date of assessment(mean)` = mean(date_of_assessment, na.rm = TRUE),
      `date of assessment (sd)`  = sd(date_of_assessment, na.rm = TRUE),
      `autism score (mean)`      = mean({{phenotype}}, na.rm = TRUE),
      sd        = sd({{phenotype}},   na.rm = TRUE),
      .groups   = "drop"
    ) 
    # %>%
  #   left_join(pvals, by = "rater") %>%
  #   left_join(effect_sizes, by = "rater")

  # descriptives$p_value = descriptives$p_value * 4 # Bonferroni correction for 4 tests

  return(descriptives)
}

calculate_descriptives_genotype <- function(data, PGS) {
  # filter on unique FISNumber to avoid duplicates in the data (as the data is in long format with multiple rows per participant)
  data_unique = data %>%
    group_by(`FISNumber`) %>%
    dplyr::slice(1) %>%
    ungroup() 

  # calculate descriptives for each rater type and sex
  descriptives <- data_unique %>%
    group_by(`sex`) %>%
    summarise(
      name      = "PGS",
      n         = sum(!is.na({{PGS}})),
      `PGS (mean)`      = mean({{PGS}}, na.rm = TRUE),
      sd        = sd({{PGS}},   na.rm = TRUE),
      skewness  = skewness({{PGS}},  na.rm = TRUE),
      kurtosis  = kurtosis({{PGS}},  na.rm = TRUE),
      .groups   = "drop"
    ) 
  
  return(descriptives)
}

# correlation matrix of all raters separated on sex
data_wide = data %>%
  pivot_wider(names_from = sex, values_from = c(m12_aut_sum, v12_aut_sum, t12_aut_sum, ysr14_aut_sum))

cor_matrix_females = cor(select(data_wide, m12_aut_sum_Female, v12_aut_sum_Female, t12_aut_sum_Female, ysr14_aut_sum_Female), use = "pairwise.complete.obs")
cor_matrix_males = cor(select(data_wide, m12_aut_sum_Male, v12_aut_sum_Male, t12_aut_sum_Male, ysr14_aut_sum_Male), use = "pairwise.complete.obs")
cor_all = cor(select(data, m12_aut_sum, v12_aut_sum, t12_aut_sum, ysr14_aut_sum), use = "pairwise.complete.obs")
cor_matrix_with_sensitivity = cor(select(data, m12_aut_sum, m12_aut_sum_sensitivity, v12_aut_sum, v12_aut_sum_sensitivity, t12_aut_sum, t12_aut_sum_sensitivity, ysr14_aut_sum, ysr14_aut_sum_sensitivity), use = "pairwise.complete.obs")

cor_all = tibble::rownames_to_column(as.data.frame(cor_all), var = "variable")
cor_matrix_females = tibble::rownames_to_column(as.data.frame(cor_matrix_females), var = "variable")
cor_matrix_males = tibble::rownames_to_column(as.data.frame(cor_matrix_males), var = "variable")

# count the number of individuals for which each rater type is available for that individual
data_overlap = data %>%
  filter(!is.na(m12_aut_sum) & !is.na(v12_aut_sum) & !is.na(t12_aut_sum) & !is.na(ysr14_aut_sum))

data_overlap_no_father = data %>%
  filter(!is.na(m12_aut_sum) & !is.na(t12_aut_sum) & !is.na(ysr14_aut_sum))

# --- show / save results ---

descriptives_phenotype <- calculate_descriptives_phenotype(data_long, `autism_score`)
descriptives_genotype <- calculate_descriptives_genotype(data, `PGS`)

# descriptives_phenotype
# descriptives_genotype

# cor_matrix_females
# cor_matrix_males

# append descriptives with data_overlap sample size
descriptives_phenotype = descriptives_phenotype %>%
  bind_rows(
    data.frame(
      rater = "overlap all raters",
      sex = c("Male", "Female"),
      n = c(nrow(data_overlap[data_overlap$sex == "Male", ]), nrow(data_overlap[data_overlap$sex == "Female", ]))))


# --- collect results ---

lst_correlations = list(
  `Phenotype correlations` = cor_all,
  cor_matrix_females = cor_matrix_females,
  cor_matrix_males   = cor_matrix_males
)

lst_descriptives = list(
  `Sample descriptives` = descriptives_phenotype,
  `Genotype descriptives` = descriptives_genotype
)

# save files
openxlsx::write.xlsx(lst_correlations, "results/correlation_matrices.xlsx", rowNames = FALSE)
openxlsx::write.xlsx(lst_descriptives, "results/descriptives.xlsx", rowNames = FALSE)
