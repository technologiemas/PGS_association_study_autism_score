# This script generates visualizations for linear models 
# These need different plot types due to the nature of linear regression outputs.
# TODO: to be deleted

rm(list = ls(all = TRUE))
gc()

library(ggplot2)
library(emmeans)
library(dplyr)
library(tidyr)
library(marginaleffects)
library(llme)

# Load linear models
# Assuming fit_m3_linear and fit_m4_linear are now 'lm' or 'lmer' objects
fit_m3_linear <- readRDS("results/models/fit_m3_linear.rds")
fit_m4_linear <- readRDS("results/models/fit_m4_linear.rds")
fit_m5_linear <- readRDS("results/models/fit_m5_linear.rds")

colors = c("Male" = "#00C07B", "Female" = "#FFBB09")
dir.create("results/figures/linear", showWarnings = FALSE, recursive = TRUE)

# --- Two-way interaction: rater * sex ---
# 1. Get Marginal Means (Predicted Scores)
emm_rater_sex <- emmeans(fit_m3_linear, ~ sex_effect | rater)
rater_sex_df <- as.data.frame(emm_rater_sex)

# 2. Plot Predicted Means
ggplot(rater_sex_df, aes(x = rater, y = emmean, color = sex_effect, group = sex_effect)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.5)) +
  # facet_grid(~ rater) +
  scale_color_manual(values = colors) +
  theme(legend.position = "top") +
  labs(y = "Predicted Autism Score (scaled; 95% CI)", 
       x = "Rater Type",
       caption = "Estimated at the population mean of covariates. Based on linear model 3.") +
  theme_minimal()

ggsave("results/figures/linear/rater_sex_linear.tiff", device = "tiff", width = 8.4, height = 6, units = "cm", dpi = 600, scale = 2)


# --- Three-way interaction: PGS * rater * sex ---
# 1. Get trends over the continuous PGS variable
rg <- emmeans(fit_m4_linear, ~ sex_effect | PGS_scaled * rater, 
              at = list(PGS_scaled = seq(-3, 3, by = 0.1)))
rater_sex_pgs <- as.data.frame(rg)

# 2. Plotting the slopes
ggplot(rater_sex_pgs, aes(x = PGS_scaled, y = emmean, color = sex_effect, fill = sex_effect)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.1, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ rater, ncol = 2) + 
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(title = "Effect of PGS on Autism Score by Rater and Sex",
       x = "PGS (SD)",
       y = "Predicted Autism Score (SD)",
       color = "Sex") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("results/figures/linear/slopes_three_way_interaction_linear.tiff", device = "tiff", width = 8.4, height = 10, units = "cm", dpi = 600, scale = 2)

# statistical test for slopes of PGS
emt <- emtrends(fit_m4_linear, ~ rater * sex_effect, var = "PGS_scaled")
contrast(emt, method = "pairwise", by = "rater", adjust = "fdr")

