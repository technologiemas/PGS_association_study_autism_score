rm(list = ls(all = TRUE))
gc()

library(ordinal)
library(ggplot2)
library(emmeans)
library(dplyr)

# Load the the clmm model with the best fit (model 3)
fit_m3 <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
fit_m4 <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds")

fit_m3 = readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds")
fit_m4 = readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds")

# colors to use
colors = c("Male" = "#00C07B", "Female" = "#FFBB09")

# --- Two-way interaction of the significant finding rater_type * sex ---
# 1. Emmeans to get predicted probabilities for sex by rater type across autism score ordinal levels
emm_prob_rater_sex <- emmeans(fit_m3, ~ sex * autism_score_ordinal_sensitivity | rater_type, # "within each rater what is the prob for the autism score levels?"
              mode = "prob",
              cov.reduce = mean, # this averages over PGS and other covariates
              )
rater_sex_df <- as.data.frame(emm_prob_rater_sex)

# Pairwise contrasts of predicted probabilities (males - females)
contr_per_rater_type <- contrast(
  emm_prob_rater_sex,
  method = "pairwise",
  by = c("rater_type", "autism_score_ordinal_sensitivity"),
  adjust = "none"
)
# add columns for lower and upper bounds of 95% CI
contr_df <- summary(contr_per_rater_type, type = "response") %>%
  as.data.frame() %>%
  mutate(
    lower = estimate - 1.96 * SE,
    upper = estimate + 1.96 * SE,
    diff_prob = estimate
  )

# 2. Rename and adjust factor levels for plotting
rater_sex_df$rater_type <- factor(rater_sex_df$rater_type, 
                  levels = c("m12", "v12", "t12", "ysr14"), 
                  labels = c("Mother", "Father", "Teacher", "Self"))
rater_sex_df$sex <- factor(rater_sex_df$sex, 
                 levels = c("MALE", "FEMALE"), 
                 labels = c("Male", "Female"))
rater_sex_df$autism_score_ordinal_sensitivity <- factor(rater_sex_df$autism_score_ordinal_sensitivity,
                                   levels = c(1, 2, 3),
                                   labels = c("No", "Low", "High"))

contr_df <- contr_df %>%
  mutate(
    rater_type = factor(rater_type, 
                        levels = c("m12", "v12", "t12", "ysr14"), 
                        labels = c("Mother", "Father", "Teacher", "Self")),
    autism_score_ordinal_sensitivity = factor(autism_score_ordinal_sensitivity,
                                  levels = c(1, 2, 3),
                                  labels = c("No", "Low", "High"))
  )                                   

# 3. Plot predicted probabilities with ggplot2
ggplot(rater_sex_df, aes(x = as.factor(autism_score_ordinal_sensitivity), y = prob, color = sex, group = sex)) +
  geom_line(position = position_dodge(width = 0.3)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, position = position_dodge(width = 0.3)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(~ rater_type) +
  scale_color_manual(values = colors) +
  labs(y = "Predicted probability (95% CI)", 
  x = "Autism score level", 
  caption = "Predicted probabilities are estimated at the population mean of PGS (PGS = 0). \nBased on model 3.") +
  theme(legend.position = "top")

# save the figure
ggsave("results/figures/sensitivity/pred_prob_rater_sex_sensitivity.tiff", device = "tiff", width = 8.4, height = 6, units = "cm", dpi = 600, scale = 2)


# 3b. Plotting the pairwise contrasts of predicted probabilities
ggplot(contr_df, aes(x = diff_prob, y = factor(rater_type,
                                               levels = c("Self", "Teacher", "Father", "Mother")))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") + # null line
  geom_point(aes(color = autism_score_ordinal_sensitivity), size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, color = autism_score_ordinal_sensitivity), height = 0.2) +
  # facet_wrap(~ contrast) +  # e.g., Male-Female
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Difference in predicted probability (95% CI)",
    y = "Rater",
    color = "Autism score level",
    title = "Pairwise contrasts of predicted probabilities",
    caption = "Based on model 3",
    subtitle = "Differences are Male − Female for each rater and outcome level \nsuch that positive values indicate higher predicted probabilities for Males"
  ) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

ggsave("results/figures/sensitivity/pairwise_contrast_rater_sex_sensitivity.tiff", device = "tiff", width = 8.4, height = 6, units = "cm", dpi = 600, scale = 2)


# Cumulative Predicted probabilities of belonging to each score category of both sexes by rater type
# emmip(
#   fit_m3,
#   ~ sex ~ cut | rater_type,
#   mode = "cum.prob",
#   dodge = 0.2,
#   CIs = TRUE
# ) +
#   theme(legend.position = "top") +
#   scale_y_continuous(labels = scales::percent) +
#   ylab("Cumulative Probability")


# --- Three-way interaction effects visualization ---

# 1. Get predicted probabilities for three way interaction using emmeans
rg <- emmeans(fit_m4, ~ sex * autism_score_ordinal_sensitivity | PGS_scaled * rater_type, 
              mode = "prob", 
              at = list(PGS_scaled = seq(-3, 3, by = 0.1)))
rater_sex_pgs <- as.data.frame(rg)

# Rename and adjust factor levels for plotting
rater_sex_pgs$rater_type <- factor(rater_sex_pgs$rater_type, 
                        levels = c("m12", "v12", "t12", "ysr14"),
                        labels = c("Mother", "Father", "Teacher", "Self"))
rater_sex_pgs$autism_score_ordinal_sensitivity <- factor(rater_sex_pgs$autism_score_ordinal_sensitivity,
                                   levels = c(1, 2, 3),
                                   labels = c("No", "Low", "High"))
rater_sex_pgs$sex <- factor(rater_sex_pgs$sex, 
                 levels = c("MALE", "FEMALE"), 
                 labels = c("Male", "Female"))

# 2. Plot with Sex and Score Level together
ggplot(rater_sex_pgs, aes(x = PGS_scaled, y = prob, 
               color = sex, 
               linetype = autism_score_ordinal_sensitivity,
               group = interaction(autism_score_ordinal_sensitivity, sex))) +
  # Confidence intervals 
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = as.factor(sex)), 
              alpha = 0.1, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1) +
  # Facet by rater_type
  facet_wrap(~ rater_type, ncol = 2) + 
  scale_linetype_manual(values = c("solid", "longdash", "dotted")) + 
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Three-way interaction predicting three levels of autism score",
       subtitle = "PGS * Rater * Sex",
       x = "PGS (SDs; 95% CI)",
       y = "Predicted Probability (95% CI)",
       color = "Sex",
       caption = "Based on model 4.",
       linetype = "Autism score level") +
  theme(legend.position = "bottom", 
        legend.box = "vertical")

# save plot
ggsave("results/figures/sensitivity/pred_prob_three_way_interaction_sensitivity.tiff", device = "tiff", width = 8.4, height = 10, units = "cm", dpi = 600, scale = 2)

# 3. Plot for high score only as this is relevant considering an autism diagnosis is based on "high scores"
# filtering on only high score
rater_sex_pgs_high <- subset(rater_sex_pgs, autism_score_ordinal_sensitivity == "High")

ggplot(rater_sex_pgs_high, aes(x = PGS_scaled, y = prob, 
               color = as.factor(sex), 
               linetype = autism_score_ordinal_sensitivity,
               group = interaction(autism_score_ordinal_sensitivity, sex))) +
  # Confidence intervals (optional: can get messy with too many lines)
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = as.factor(sex)), 
              alpha = 0.1, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1) +
  # Facet by rater_type
  facet_wrap(~ rater_type, ncol = 2) + 
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent) +
  labs(title = "Three-way interaction predicting high autism score",
       subtitle = "PGS * Rater * Sex",
       x = "PGS (SDs; 95% CI)",
       y = "Predicted Probability",
       color = "Sex",
       caption = "Based on model 4.",
       linetype = "Autism score level") +
  theme(legend.position = "bottom", 
        legend.box = "vertical")

ggsave("results/figures/sensitivity/pred_prob_three_way_interaction_high_only_sensitivity.tiff", device = "tiff", width = 8.4, height = 10, units = "cm", dpi = 600, scale = 2)


# --- Forest plot of Odds Ratios for main and interaction effects ---
# I think this is not the way to do it however...

# 1. Extract coefficients and confidence intervals
# coef_summary <- as.data.frame(summary(fit_m3)$coefficients)
coef_summary <- as.data.frame(summary(fit_m4)$coefficients)

# Filter out unwanted items
fixed_effects <- coef_summary[!grepl("PLATFORM|\\(Intercept\\)|\\||PC", rownames(coef_summary)), ]
                        
# Calculate Odds Ratios and 95% CIs
plot_data <- data.frame(
  term = rownames(fixed_effects),
  estimate = exp(fixed_effects$Estimate),
  conf.low = exp(fixed_effects$Estimate - 1.96 * fixed_effects$`Std. Error`),
  conf.high = exp(fixed_effects$Estimate + 1.96 * fixed_effects$`Std. Error`),
  sig_code = case_when(fixed_effects$`Pr(>|z|)` < 0.001 ~ "***",
                      fixed_effects$`Pr(>|z|)` < 0.01 ~ "**",
                      fixed_effects$`Pr(>|z|)` < 0.05 ~ "*",
                      TRUE ~ "")
)

# 2. Clean up term names for better readability
plot_data_clean <- plot_data %>%
  mutate(term = case_when(
    term == "PGS_scaled" ~ "PGS (Main Effect)",
    term == "sexMale" ~ "Sex (Male)",
    term == "sexFEMALE" ~ "Sex (Female)",
    term == "rater_typev12" ~ "Rater: Father",
    term == "rater_typet12" ~ "Rater: Teacher",
    term == "rater_typeysr14" ~ "Rater: Self",
    term == "PGS_scaled:sexFEMALE" ~ "PGS × Female",
    term == "PGS_scaled:rater_typet12" ~ "PGS × Teacher",
    term == "PGS_scaled:rater_typev12" ~ "PGS × Father",
    term == "PGS_scaled:rater_typeysr14" ~ "PGS × Self",
    term == "rater_typev12:sexFEMALE" ~ "Female × Father",
    term == "rater_typet12:sexFEMALE" ~ "Female × Teacher",
    term == "rater_typeysr14:sexFEMALE" ~ "Female × Self",
    term == "age_scaled" ~ "Age (Scaled)",
    term == 'PGS_scaled:rater_typet12:sexFEMALE' ~ "PGS × Teacher × Female",
    term == 'PGS_scaled:rater_typev12:sexFEMALE' ~ "PGS × Father × Female",
    term == 'PGS_scaled:rater_typeysr14:sexFEMALE' ~ "PGS × Self × Female",
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

# 3. Create the forest plot
ggplot(plot_data_clean, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 3, color = "steelblue") +
  # Add p-values:
  geom_text(aes(x = 0.5, y = term, label = sig_code), 
            color = "black", 
            size = 5, 
            hjust = 1, # Adjust this to move them slightly right of the axis line
            vjust = 0.7,
            # fontface = "bold",
            show.legend = FALSE) +
  scale_x_log10() + # Odds Ratios are best viewed on a log scale
  # layout:
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  labs(title = "Odds Ratios for higher Autism Score",
       subtitle = "Estimates > 1 indicate higher probability of 'Low' or 'High' autism scores \nReference groups: Male, Mother rater",
       x = "Odds Ratio (95% CI) on log10 scale",
       caption = "Based on model 4.",
       y = NULL)

# save the figure
ggsave("results/figures/sensitivity/odds_ratios_forest_plot_three_way_sensitivity.tiff", device = "tiff", width = 8.4, height = 12, units = "cm", dpi = 600, scale = 2)

