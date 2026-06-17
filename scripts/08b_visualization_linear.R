# This script generates visualizations for linear models 
# These need different plot types due to the nature of linear regression outputs.

rm(list = ls(all = TRUE))
gc()

library(ggplot2)
library(emmeans)
library(dplyr)
library(tidyr)
library(marginaleffects)

# Load linear models
# Assuming fit_m3_linear and fit_m4_linear are now 'lm' or 'lmer' objects
fit_m3_linear <- readRDS("results/models/fit_m3_linear.rds")
fit_m4_linear <- readRDS("results/models/fit_m4_linear.rds")

colors = c("Male" = "#00C07B", "Female" = "#FFBB09")
dir.create("results/figures/linear", showWarnings = FALSE, recursive = TRUE)

# --- Two-way interaction: rater * sex ---
# 1. Get Marginal Means (Predicted Scores)
emm_rater_sex <- emmeans(fit_m3_linear, ~ sex | rater)
rater_sex_df <- as.data.frame(emm_rater_sex)

# 2. Plot Predicted Means
ggplot(rater_sex_df, aes(x = rater, y = emmean, color = sex, group = sex)) +
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
rg <- emmeans(fit_m4_linear, ~ sex | PGS_scaled * rater, 
              at = list(PGS_scaled = seq(-3, 3, by = 0.1)))
rater_sex_pgs <- as.data.frame(rg)

# 2. Plotting the slopes
ggplot(rater_sex_pgs, aes(x = PGS_scaled, y = emmean, color = sex, fill = sex)) +
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


# --- Forest Plot (Coefficient Estimates) ---
# For linear models, we use the raw Estimate (Beta) instead of exp(Estimate)
coef_summary <- as.data.frame(summary(fit_m4_linear)$coefficients)

# In lm, the column name is usually "Estimate" and "Pr(>|t|)" 
# (Adjust if using lmer which might not have p-values by default)
plot_data <- coef_summary %>%
  filter(!grepl("PLATFORM|\\(Intercept\\)|PC", rownames(.))) %>%
  mutate(
    term = rownames(.),
    estimate = Estimate,
    conf.low = Estimate - 1.96 * `Std. Error`,
    conf.high = Estimate + 1.96 * `Std. Error`,
    sig_code = case_when(
      `Pr(>|t|)` < 0.001 ~ "***",
      `Pr(>|t|)` < 0.01 ~ "**",
      `Pr(>|t|)` < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# 2. Clean up term names for better readability
plot_data_clean <- plot_data %>%
  mutate(term = case_when(
    term == "PGS_scaled" ~ "PGS (Main Effect)",
    term == "sexMale" ~ "Sex (Male)",
    term == "sexFEMALE" ~ "Sex (Female)",
    term == "raterv12" ~ "Rater: Father",
    term == "ratert12" ~ "Rater: Teacher",
    term == "raterysr14" ~ "Rater: Self",
    term == "PGS_scaled:sexFEMALE" ~ "PGS × Female",
    term == "PGS_scaled:ratert12" ~ "PGS × Teacher",
    term == "PGS_scaled:raterv12" ~ "PGS × Father",
    term == "PGS_scaled:raterysr14" ~ "PGS × Self",
    term == "raterv12:sexFEMALE" ~ "Female × Father",
    term == "ratert12:sexFEMALE" ~ "Female × Teacher",
    term == "raterysr14:sexFEMALE" ~ "Female × Self",
    term == "age_scaled" ~ "Age (Scaled)",
    term == 'PGS_scaled:ratert12:sexFEMALE' ~ "PGS × Teacher × Female",
    term == 'PGS_scaled:raterv12:sexFEMALE' ~ "PGS × Father × Female",
    term == 'PGS_scaled:raterysr14:sexFEMALE' ~ "PGS × Self × Female",
    TRUE ~ term 
  )) %>%
  # reorder terms so that main effects are first and interactions after
    mutate(term = factor(term, levels = rev(c(
        "PGS (Main Effect)",
        "Sex (Female)",
        "Rater: Father",
        "Rater: Teacher",
        "Rater: Self",
        "Age (Scaled)",
        "PGS × Female",
        "PGS × Teacher",
        "PGS × Father",
        "PGS × Self",
        "Female × Father",
        "Female × Teacher",
        "Female × Self",
        "PGS × Teacher × Female",
        "PGS × Father × Female",
        "PGS × Self × Female"
        ))))
        
# 3. Create Forest Plot (Linear Scale)
ggplot(plot_data_clean, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + # Null is 0 for LM
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(aes(x = min(conf.low) - 0.1, label = sig_code), size = 5) +
  labs(title = "Linear Regression Coefficients",
       x = "Estimate (Beta) with 95% CI",
       y = NULL) +
  theme_minimal()

ggsave("results/figures/linear/betas_forest_plot_three_way_linear.tiff", device = "tiff", width = 8.4, height = 12, units = "cm", dpi = 600, scale = 2)
