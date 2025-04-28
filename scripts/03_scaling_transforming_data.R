library(ggplot2)
library(moments)
library(bestNormalize)
library(pscl)

data = readRDS("data/processed/03_data_mother_clean.rds")

# plot the distribution of the variable m12_aut_sum
ggplot(data, aes(x = m12_aut_sum)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of m12_aut_sum", x = "m12_aut_sum", y = "Count") +
  theme_minimal()

# scale the variable m12_aut_sum
data$m12_aut_sum_scaled = scale(data$m12_aut_sum)
ggplot(data, aes(x = m12_aut_sum_scaled)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of m12_aut_sum_scaled", x = "m12_aut_sum_scaled", y = "Count") +
  theme_minimal()

# take log of the variable m12_aut_sum to remove skew
data$m12_aut_sum_log = log(data$m12_aut_sum)
ggplot(data, aes(x = m12_aut_sum_log)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of m12_aut_sum_log", x = "m12_aut_sum_log", y = "Count") +
  theme_minimal()

# take square root of the variable m12_aut_sum to remove skew
data$m12_aut_sum_sqrt = sqrt(data$m12_aut_sum)
ggplot(data, aes(x = m12_aut_sum_sqrt)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of m12_aut_sum_sqrt", x = "m12_aut_sum_sqrt", y = "Count") +
  theme_minimal()

# take 1/x of the variable m12_aut_sum to remove skew
data$m12_aut_sum_inv = 1 / data$m12_aut_sum
ggplot(data, aes(x = m12_aut_sum_inv)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of m12_aut_sum_inv", x = "m12_aut_sum_inv", y = "Count") +
  theme_minimal()

# take inverted normalization transformation (INT) while many of the values are 0
data$m12_aut_sum_qnorm = qnorm((rank(data$m12_aut_sum, na.last = "keep") - 0.5) / (length(data$m12_aut_sum) - sum(is.na(data$m12_aut_sum))))
data$m12_aut_sum_qnorm = scale(data$m12_aut_sum_qnorm) # check the scaling
ggplot(data, aes(x = m12_aut_sum_qnorm)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of m12_aut_sum_qnorm", x = "m12_aut_sum_qnorm", y = "Count") +
  theme_minimal()

# bestnormalize package
norm_result <- bestNormalize(data$m12_aut_sum)
data$m12_aut_sum_bestnorm = norm_result$x.t
ggplot(data, aes(x = m12_aut_sum_bestnorm)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of m12_aut_sum_bestnorm", x = "m12_aut_sum_bestnorm", y = "Count") +
  theme_minimal()

# check skewness for each
skewness(data$m12_aut_sum, na.rm = TRUE)
skewness(data$m12_aut_sum_scaled, na.rm = TRUE)
skewness(data$m12_aut_sum_log, na.rm = TRUE)
skewness(data$m12_aut_sum_sqrt, na.rm = TRUE)
skewness(data$m12_aut_sum_inv, na.rm = TRUE)
skewness(data$m12_aut_sum_qnorm, na.rm = TRUE)
skewness(data$m12_aut_sum_bestnorm, na.rm = TRUE)

# ------------------
# test homoscedasticity, normality, residuals

diagnostics_check <- function(model) {
  library(car)
  library(lmtest)
  library(nortest)
  library(ggplot2)

  bp_test <- bptest(model) # Breusch-Pagan test for homoscedasticity
 
  residuals <- model$residuals
  residuals_df <- data.frame(residuals = residuals)

  # test for normality of residuals using anderson-darling test
  ad_test <- ad.test(residuals)


 print(paste("BP test p-value: ", bp_test$p.value))
  if (bp_test$p.value < 0.05) {
    print("Heteroscedasticity detected")
  } else {
    print("No heteroscedasticity detected")
  }

  print(paste("Anderson-Darling test p-value:", ad_test$p.value))
   if (ad_test$p.value < 0.05) {
    print("No normality detected")
  } else {
    print("Normality detected")
  }

  print(paste("skewness of residuals:", skewness(residuals, na.rm = TRUE)))
  print(paste("kurtosis of residuals:", kurtosis(residuals, na.rm = TRUE)))

  par(mfrow = c(2, 2))
  plot(model)
}

model = lm(m12_aut_sum ~ P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1, data = data)
model_log = lm(m12_aut_sum_log ~ P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1, data = data)
model_qnorm = lm(m12_aut_sum_qnorm ~ P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1, data = data)
model_bestnorm = lm(m12_aut_sum_bestnorm ~ P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1, data = data)
model_sqrt = lm(m12_aut_sum_sqrt ~ P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1, data = data)
model_INT = lm(m12_aut_sum_qnorm ~ P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1, data = data)
model_zero_inflated_poisson = zeroinfl(m12_aut_sum ~ P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1, data = data, dist = "poisson")

diagnostics_check(model)
diagnostics_check(model_log)
diagnostics_check(model_qnorm)
diagnostics_check(model_bestnorm)
diagnostics_check(model_sqrt)
diagnostics_check(model_INT)

# plot the residuals of the model
ggplot(data, aes(x = model_INT$residuals)) +
  geom_histogram(binwidth = 0.1) +
  labs(title = "Residuals of the model", x = "Residuals", y = "Count") +
  theme_minimal()

# plot the model
ggplot(data, aes(x = pgs_scaled, y = m12_aut_sum_scaled)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Model of m12_aut_sum_qnorm vs P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1", x = "P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1", y = "m12_aut_sum_qnorm") +
  theme_minimal()

# ------------------
# checking distribution of genotypical data

data$pgs_scaled = scale(data$P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1)

# plot the distribution of the variable P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1
ggplot(data, aes(x = P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1)) +
  geom_histogram() +
  labs(title = "Distribution of P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1", x = "P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1", y = "Count") +
  theme_minimal()


# randomly sample 5000 from the residuals of the model
set.seed(123)
model_sample = sample(model$residuals, 5000, replace = TRUE)

# shapiro-wilk test for normality
shapiro_test = shapiro.test(model_sample)
print(paste("Shapiro-Wilk test p-value: ", shapiro_test$p.value))

