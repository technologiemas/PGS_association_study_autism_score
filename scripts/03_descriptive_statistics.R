rm(list = ls(all = TRUE))
gc()

library(ggplot2)
library(moments)
library(bestNormalize)
library(pscl)
library(dplyr)
library(tidyr)
library(ggpubr)
library(psych)

data = readRDS("data/processed/02_full_dataset_clean.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")
# data_father = readRDS("data/processed/02_data_father_clean.rds")
data_mother = readRDS("data/processed/02_data_mother_clean.rds")
# data_self = readRDS("data/processed/02_data_ysr_clean.rds")
# data_teacher = readRDS("data/processed/02_data_teacher_clean.rds")


# --- distribution plots ---
# plot the distribution of the phenotype data
# ggplot(data, aes(x = m12_aut_sum)) +
#   geom_histogram(binwidth = 0.1) +
#   labs(title = "Distribution of mother aut sum", x = "m12_aut_sum", y = "Count") 

# ggplot(data, aes(x = ysr14_aut_sum)) +
#   geom_histogram(binwidth = 0.1) +
#   labs(title = "Distribution of self aut sum", x = "ysr14_aut_sum", y = "Count")

# ggplot(data, aes(x = t12_aut_sum)) +
#   geom_histogram(binwidth = 0.1) +
#   labs(title = "Distribution of teacher aut sum", x = "t12_aut_sum", y = "Count") 

# table(data$genderlkrt12)
# table(data$sex)


# --- descriptives and statistical tests ---
calculate_descriptives <- function(data_long, variable) {
  # Calculate descriptive statistics and perform t-tests for a given variable.
  #
  # Args:
  #   data_long: A data frame containing the data in long format
  #   variable: The variable for which to calculate descriptives and perform t-tests.
  #
  # Returns:
  #   A data frame with descriptive statistics and p-values from t-tests.

  # one t-test per rater_type
  pvals <- data_long %>%
    group_by(`rater_type`) %>%
    summarise(t_test_p = t.test({{variable}} ~ `sex`)$p.value,
              .groups = "drop")

  # calculate effect sizes
  effect_sizes <- data_long %>%
    group_by(`rater_type`) %>%
    reframe(
      cohen_d = cohen.d({{variable}} ~ `sex`, data = cur_data())$cohen.d[2],
    )

  # calculate descriptives for each rater type and sex
  descriptives <- data_long %>%
    group_by(`rater_type`, `sex`) %>%
    summarise(
      n         = sum(!is.na({{variable}})),
      mean      = mean({{variable}}, na.rm = TRUE),
      sd        = sd({{variable}},   na.rm = TRUE),
      skewness  = skewness({{variable}},  na.rm = TRUE),
      kurtosis  = kurtosis({{variable}},  na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    left_join(pvals, by = "rater_type") %>%
    left_join(effect_sizes, by = "rater_type")

  return(descriptives)
}

data_long$P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1

# correlation matrix of all raters separated on sex
data_wide = data %>%
  pivot_wider(names_from = sex, values_from = c(m12_aut_sum, v12_aut_sum, t12_aut_sum, ysr14_aut_sum))

cor_matrix = cor(select(data_wide, m12_aut_sum_FEMALE, m12_aut_sum_MALE, v12_aut_sum_FEMALE, v12_aut_sum_MALE, t12_aut_sum_FEMALE, t12_aut_sum_MALE, ysr14_aut_sum_FEMALE, ysr14_aut_sum_MALE), use = "pairwise.complete.obs")


# --- boxplots of phenotype data ---
## box plot of mother rated phenotype separated on sex
# ggplot(data_mother, aes(x = sex, y = m12_aut_sum)) +
#   geom_boxplot() +
#   # add p_value from descriptives
#   labs(title = "Mother rated autism score by sex",
#        subtitle = paste("p =", round(pvals$t_test_p[pvals$rater_type == "m12"], 3)))

# box plot of all raters separated on sex
ggplot(data_long, aes(x = sex, y = autism_score)) +
  geom_boxplot() +
  facet_wrap(~ rater_type) + 
  stat_compare_means(method = "t.test", 
                     bracket.size = 0.7,
                     size= 7,
                     label = "p.signif",      # Use stars: *, **, ***
                     comparisons = list(c("FEMALE", "MALE")),  # Adjust to your factor levels
                     hide.ns = TRUE)    +      # Hide non-significant comparisons
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Autism Score by Rater Type and Sex",
       subtitle = "Comparison of Female and Male Ratings")

# box plot of genotype
ggplot(data_long, aes(x = sex, y = P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1)) +
  geom_boxplot() +
  stat_compare_means(method = "t.test", 
                     bracket.size = 0.7,
                     size= 7,
                     label = "p.signif",      # Use stars: *, **, ***
                     comparisons = list(c("FEMALE", "MALE"))  # Adjust to your factor levels
                     )    +
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Genotype by Sex",
       subtitle = "Comparison of Female and Male Ratings")

# --- save results ---
descriptives_phenotype <- calculate_descriptives(data_long, `autism_score`)
descriptives_genotype <- calculate_descriptives(data_long, `P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1`)

descriptives_phenotype
descriptives_genotype
cor_matrix


