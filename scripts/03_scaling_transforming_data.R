library(ggplot2)
library(moments)
library(bestNormalize)
library(pscl)
library(dplyr)

data = readRDS("data/processed/02_full_dataset_clean.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")
data_father = readRDS("data/processed/02_data_father_clean.rds")
data_mother = readRDS("data/processed/02_data_mother_clean.rds")
data_self = readRDS("data/processed/02_data_ysr_clean.rds")
data_teacher = readRDS("data/processed/02_data_teacher_clean.rds")

# plot the distribution of the variable m12_aut_sum
ggplot(data_mother, aes(x = m12_aut_sum)) +
  geom_histogram(binwidth = 0.1) +
  labs(title = "Distribution of mother aut sum", x = "m12_aut_sum", y = "Count") 

ggplot(data_self, aes(x = ysr14_aut_sum)) +
  geom_histogram(binwidth = 0.1) +
  labs(title = "Distribution of self aut sum", x = "ysr14_aut_sum", y = "Count")

ggplot(data_teacher, aes(x = t12_aut_sum)) +
  geom_histogram(binwidth = 0.1) +
  labs(title = "Distribution of teacher aut sum", x = "t12_aut_sum", y = "Count") 

# check skewness for each
skewness(data_mother$m12_aut_sum, na.rm = TRUE)
skewness(data_father$f12_aut_sum, na.rm = TRUE)
skewness(data_self$ysr14_aut_sum, na.rm = TRUE)
skewness(data_teacher$t12_aut_sum, na.rm = TRUE)
# check kurtosis for each
kurtosis(data_mother$m12_aut_sum, na.rm = TRUE)
kurtosis(data_father$f12_aut_sum, na.rm = TRUE)
kurtosis(data_self$ysr14_aut_sum, na.rm = TRUE)
kurtosis(data_teacher$t12_aut_sum, na.rm = TRUE)


phdatL_e1 = data_self %>%
  select(FamilyNumber, ysr14_aut_sum, P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1) %>%
  rename(ph = ysr14_aut_sum,
         pgst = P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1,
         famnr = FamilyNumber)

# --- boxplots of phenotype data ---
# box plot of mother rated phenotype separated on sex
ggplot(data_mother, aes(x = sex, y = m12_aut_sum)) +
  geom_boxplot()

# box plot of all raters separated on sex
ggplot(data_long, aes(x = sex, y = autism_score)) +
  geom_boxplot() +
  facet_wrap(~ rater_type)

# plot descriptives of all raters separated on sex
descriptives = data_long %>%
  group_by(rater_type, sex) %>%
  reframe(
    mean = mean(autism_score, na.rm = TRUE),
    sd = sd(autism_score, na.rm = TRUE),
    n = sum(!is.na(autism_score)),
    skewness = skewness(autism_score, na.rm = TRUE),
    kurtosis = kurtosis(autism_score, na.rm = TRUE),
    median = median(autism_score, na.rm = TRUE)
  )

# correlation matrix of all raters separated on sex
data_wide = data %>%
  pivot_wider(names_from = sex, values_from = c(m12_aut_sum, v12_aut_sum, t12_aut_sum, ysr14_aut_sum))

cor_matrix = cor(select(data_wide, m12_aut_sum_FEMALE, m12_aut_sum_MALE, v12_aut_sum_FEMALE, v12_aut_sum_MALE, t12_aut_sum_FEMALE, t12_aut_sum_MALE, ysr14_aut_sum_FEMALE, ysr14_aut_sum_MALE), use = "pairwise.complete.obs")

# pairwise t-tests of all raters separated on sex
groups = c("m12_aut_sum_FEMALE", "m12_aut_sum_MALE", "v12_aut_sum_FEMALE", "v12_aut_sum_MALE", "t12_aut_sum_FEMALE", "t12_aut_sum_MALE", "ysr14_aut_sum_FEMALE", "ysr14_aut_sum_MALE")
combinations = combn(groups, 2, simplify = FALSE)
pairwise_results = lapply(combinations, function(x) {
    t_test_result = t.test(data_wide[[x[1]]], data_wide[[x[2]]], paired = FALSE, var.equal = FALSE)
    
    # Extract the p-value and confidence interval
    p_value = t_test_result$p.value
    conf_int = t_test_result$conf.int

    # add to table
    result = data.frame(
      group1 = x[1],
      group2 = x[2],
      p_value = p_value,
      conf_int_lower = conf_int[1],
      conf_int_upper = conf_int[2]
    )
})

# Combine all results into a single data frame like a matrix with group1 columns and group2 rows
pairwise_matrix = matrix(NA, nrow = length(groups), ncol = length(groups))
rownames(pairwise_matrix) = groups
colnames(pairwise_matrix) = groups

for (result in pairwise_results) {
    group1 = result$group1
    group2 = result$group2
    p_value = result$p_value
    pairwise_matrix[group1, group2] = p_value
    pairwise_matrix[group2, group1] = p_value
}


