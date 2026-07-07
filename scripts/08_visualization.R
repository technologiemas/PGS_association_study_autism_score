# This script creates visualizations based on the model results

rm(list = ls(all = TRUE))
gc()

library(scales)
library(ordinal)
library(ggplot2)
library(emmeans)
library(dplyr)
source("scripts/_helper_functions.R")



colors <- c("Male" = "#00C07B", "Female" = "#FFBB09") # set colors for the plots


# calculate emmeans and contrasts for two way interaction
get_rater_sex_emmeans <- function(model_fit, outcome_var) {

  emm <- emmeans(
    model_fit,
    as.formula(paste("~ sex *", outcome_var, "| rater")),
    mode = "prob",
    # at = list(PGS = 2), # uncomment to predict at a specific PGS value (e.g., 2 SDs above the mean). This is clinically relevant as it represents individuals with a high genetic liability for autism. If left commented, the predictions will be made at the mean PGS (PGS = 0).
    cov.reduce = mean # takes a mean of covariates for prediction, including PGS
  )

  probs <- as.data.frame(emm) %>%
    mutate(
      rater = rater,
      sex = sex,
      !!outcome_var := get(outcome_var)
    )

  contr <- contrast(
    emm,
    method = "pairwise",
    by = c("rater", outcome_var),
    adjust = "none"
  )

  contr_df <- summary(contr, type = "response") %>%
    as.data.frame() %>%
    mutate(
      lower = estimate - 1.96 * SE,
      upper = estimate + 1.96 * SE,
      diff_prob = estimate,
      rater = rater,
      !!outcome_var := get(outcome_var)
    )

  list(
    probs = probs,
    contrasts = contr_df
  )
}


# --- rater * sex plots ---

plot_pred_prob_rater_sex <- function(probs_df, model_label, outcome_var) {
  
  probs_df[[outcome_var]] <- factor(
    probs_df[[outcome_var]],
    levels = c(1, 2, 3),
    labels = c("no", "mild", "high")
  )

  ggplot(
    probs_df,
    aes(
      x = .data[[outcome_var]],
      y = prob,
      color = sex,
      group = sex
    )
  ) +
    geom_line(position = position_dodge(width = 0.3)) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      width = 0.2,
      position = position_dodge(width = 0.3)
    ) +
    facet_grid(~ rater) +
    scale_color_manual(values = colors) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      y = "Predicted probability (95% CI)",
      x = "Autism score level",
      caption = paste("Predicted at mean PGS (PGS = 0). Based on", model_label)
    ) +
    theme(
      legend.position = "top",
      legend.margin = margin(0, 0, 0, 0, "pt")
    )
}

plot_pairwise_contrasts_rater_sex <- function(contr_df, model_label, outcome_var) {

  ggplot(
    contr_df,
    aes(
      x = diff_prob,
      y = rater
    )
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_point(aes(color = get(outcome_var)), size = 3) +
    geom_errorbarh(
      aes(xmin = lower, xmax = upper, color = get(outcome_var)),
      height = 0.2
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "Difference in predicted probability (95% CI)",
      y = "Rater",
      color = paste(outcome_var, "level"),
      subtitle = "Male − Female",
      caption = paste("Based on", model_label)
    ) +
    theme_minimal()
}


# --- three way interaction plots ---


get_three_way_emmeans <- function(model_fit, pgs_range = seq(-3, 3, 0.1), outcome_var ) {

  emm <- emmeans(
    model_fit,
    as.formula(paste("~ sex *", outcome_var, "| PGS_scaled * rater")),
    mode = "prob",
    at = list(PGS_scaled = pgs_range)
  )

  as.data.frame(emm) %>%
    mutate(
      rater = rater,
      sex = sex,
      !!outcome_var := get(outcome_var)
    )
}

plot_three_way <- function(df, model_label, outcome_var) {

  ggplot(
    df,
    aes(
      x = PGS_scaled,
      y = prob,
      color = sex,
      linetype = get(outcome_var),
      group = interaction(get(outcome_var), sex)
    )
  ) +
    geom_ribbon(
      aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = sex),
      alpha = 0.1,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(linewidth = 1) +
    facet_wrap(~ rater, ncol = 2) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_linetype_manual(values = c("dotted", "longdash", "solid")) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "PGS (SDs)",
      y = "Predicted Probability (95% CI)",
      caption = paste("Based on", model_label)
    ) +
    theme(legend.position = "bottom")
}

plot_three_way_high_only <- function(df, model_label, outcome_var) {

  df_high <- subset(df, get(outcome_var) == "3")

  ggplot(
    df_high,
    aes(
      x = PGS_scaled,
      y = prob,
      color = sex,
      group = sex
    )
  ) +
    geom_ribbon(
      aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = sex),
      alpha = 0.1,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(linewidth = 1) +
    facet_wrap(~ rater, ncol = 2) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "PGS (SDs)",
      y = "Predicted Probability",
      caption = paste("Based on", model_label)
    ) +
    theme(legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0, "pt")
)
}

plot_forest_odds_ratios <- function(model_fit, model_label) {

  coef_summary <- as.data.frame(summary(model_fit)$coefficients)

  fixed <- coef_summary[
    !grepl("PLATFORM|\\(Intercept\\)|\\||PC", rownames(coef_summary)),
    , drop = FALSE
  ]

  plot_data <- data.frame(
    term = rownames(fixed),
    estimate = exp(fixed$Estimate),
    conf.low = exp(fixed$Estimate - 1.96 * fixed$`Std. Error`),
    conf.high = exp(fixed$Estimate + 1.96 * fixed$`Std. Error`),
    sig = case_when(
      fixed$`Pr(>|z|)` < 0.001 ~ "***",
      fixed$`Pr(>|z|)` < 0.01 ~ "**",
      fixed$`Pr(>|z|)` < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

  ggplot(plot_data, aes(x = estimate, y = term)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_point(size = 3) +
    geom_text(aes(label = sig), x = 0.5, hjust = 1) +
    scale_x_log10() +
    labs(
      x = "Odds Ratio (log scale)",
      y = NULL,
      caption = paste("Based on", model_label)
    ) +
    theme_minimal()
}



plot_barplot_predicted_probabilities <- function(model_fit = fit_m3, outcome_var) {
  # 1. Generate the predicted probabilities from your model
  # 'mode = "prob"' ensures we get the probability for each ordinal level

  emms_prob <- emmeans(model_fit, ~ sex * get(outcome_var)| rater, mode = "prob")
  plot_data <- as.data.frame(emms_prob)

  # 2. Create the Stacked Bar Chart
  ggplot(plot_data, aes(x = sex, y = prob, fill = factor(outcome_var))) +
    # Create the bars
    geom_col(position = "stack", color = "white", width = 0.7) +
    # Add the percentage labels inside the bars
    geom_text(
      aes(label = percent(prob, accuracy = 1)), 
      position = position_stack(vjust = 0.5), # Centers the text in each segment
      size = 4,
      fontface = "bold"
    ) +
    # Separate by rater type for side-by-side comparison
    facet_wrap(~ rater, nrow = 1) + 
    # Formatting and Colors
    scale_fill_brewer(palette = "Blues", name = "Autism Score Level") +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Predicted Autism Score Levels by Sex and Rater",
      subtitle = "Based on Ordinal Logistic Regression Probabilities",
      x = "Sex",
      y = "Predicted Probability (%)"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "bottom",
      panel.grid.major.x = element_blank()
    )
  } 


# --- load models ---

fit_m3 <- readRDS("results/models/fit_m3_clmm.rds")
fit_m4 <- readRDS("results/models/fit_m4_clmm.rds")

fit_m3_sensitivity <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
fit_m4_sensitivity <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds")

fit_m3_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds")
fit_m4_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds")

fit_m3_ysr_12 = readRDS("results/models/sensitivity/fit_ysr_12_m3_clmm_sensitivity.rds") # with the ysr at age 12 included
fit_m4_ysr_12 = readRDS("results/models/sensitivity/fit_ysr_12_m4_clmm_sensitivity.rds") 

# --- create plots ---

# main models
res_m3 <- get_rater_sex_emmeans(fit_m3, outcome_var = "autism_score_ordinal")
df_m4 <- get_three_way_emmeans(fit_m4, outcome_var = "autism_score_ordinal")

p_m3_prob <- plot_pred_prob_rater_sex(res_m3$probs, "Model 3", outcome_var = "autism_score_ordinal")
p_m3_contr <- plot_pairwise_contrasts_rater_sex(res_m3$contrasts, "Model 3", outcome_var = "autism_score_ordinal")
p_m4_three_way <- plot_three_way(df_m4, "Model 4", outcome_var = "autism_score_ordinal")
p_m4_three_way_high_only <- plot_three_way_high_only(df_m4, "Model 4", outcome_var = "autism_score_ordinal")
p_m4_forest <- plot_forest_odds_ratios(fit_m4, "Model 4")

# fit_m5 <- readRDS("results/models/fit_m5_clmm.rds")
# res_m5 <- get_rater_sex_emmeans(fit_m5, outcome_var = "autism_score_ordinal")
# p_m5_prob <- plot_pred_prob_rater_sex(res_m5$probs, "Model 5", outcome_var = "autism_score_ordinal")

# fit_age_interaction = readRDS("results/models/fit_age_interaction_clmm.rds")
# res_rater_sex <- get_rater_sex_emmeans(fit_age_interaction, outcome_var = "autism_score_ordinal")
# p_rater_sex_prob = plot_pred_prob_rater_sex(res_rater_sex$probs, "Model Rater-Sex", outcome_var = "autism_score_ordinal")

# sensitivity analyses
res_m3_sensitivity <- get_rater_sex_emmeans(fit_m3_sensitivity, outcome_var = "autism_score_ordinal_sensitivity")
df_m4_sensitivity <- get_three_way_emmeans(fit_m4_sensitivity, outcome_var = "autism_score_ordinal_sensitivity")

p_m3_prob_sensitivity <- plot_pred_prob_rater_sex(res_m3_sensitivity$probs, "Model 3", outcome_var = "autism_score_ordinal_sensitivity")
p_m3_contr_sensitivity <- plot_pairwise_contrasts_rater_sex(res_m3_sensitivity$contrasts, "Model 3", outcome_var = "autism_score_ordinal_sensitivity")
p_m4_three_way_sensitivity <- plot_three_way(df_m4_sensitivity, "Model 4", outcome_var = "autism_score_ordinal_sensitivity")
p_m4_three_way_high_only_sensitivity <- plot_three_way_high_only(df_m4_sensitivity, "Model 4", outcome_var = "autism_score_ordinal_sensitivity")
p_m4_forest_sensitivity <- plot_forest_odds_ratios(fit_m4_sensitivity, "Model 4")

# sensitivity analyses all raters present
res_m3_sensitivity_all_raters <- get_rater_sex_emmeans(fit_m3_sensitivity_all_raters, outcome_var = "autism_score_ordinal_sensitivity")
df_m4_sensitivity_all_raters <- get_three_way_emmeans(fit_m4_sensitivity_all_raters, outcome_var = "autism_score_ordinal_sensitivity")

p_m3_prob_sensitivity_all_raters <- plot_pred_prob_rater_sex(res_m3_sensitivity_all_raters$probs, "Model 3", outcome_var = "autism_score_ordinal_sensitivity")
p_m3_contr_sensitivity_all_raters <- plot_pairwise_contrasts_rater_sex(res_m3_sensitivity_all_raters$contrasts, "Model 3", outcome_var = "autism_score_ordinal_sensitivity")
p_m4_three_way_sensitivity_all_raters <- plot_three_way(df_m4_sensitivity_all_raters, "Model 4", outcome_var = "autism_score_ordinal_sensitivity")
p_m4_three_way_high_only_sensitivity_all_raters <- plot_three_way_high_only(df_m4_sensitivity_all_raters, "Model 4", outcome_var = "autism_score_ordinal_sensitivity")
p_m4_forest_sensitivity_all_raters <- plot_forest_odds_ratios(fit_m4_sensitivity_all_raters, "Model 4")


res_m3_ysr12 <- get_rater_sex_emmeans(fit_m3_ysr_12, outcome_var = "autism_score_ordinal") 
df_m4_ysr12 <- get_three_way_emmeans(fit_m4_ysr_12, outcome_var = "autism_score_ordinal") 

p_m3_prob_ysr12 <- plot_pred_prob_rater_sex(res_m3_ysr12$probs, "Model 3", outcome_var = "autism_score_ordinal")
p_m3_contr_ysr12 <- plot_pairwise_contrasts_rater_sex(res_m3_ysr12$contrasts, "Model 3", outcome_var = "autism_score_ordinal")
p_m4_three_way_ysr12 <- plot_three_way(df_m4_ysr12, "Model 4", outcome_var = "autism_score_ordinal")
p_m4_three_way_high_only_ysr12 <- plot_three_way_high_only(df_m4_ysr12, "Model 4", outcome_var = "autism_score_ordinal")


# save plots

dir.create("results/figures", showWarnings = FALSE)
dir.create("results/figures/sensitivity", showWarnings = FALSE)
dir.create("results/figures/sensitivity_all_raters", showWarnings = FALSE)

# TODO: save all relevant figures
save_plots <- function(plot, filename) {
  ggsave(
    filename,
    plot,
    device = "png",
    width = 8.4, height = 6, units = "cm",
    dpi = 600, scale = 1.4
  )
}

# save_plots(p_m3_prob_sensitivity_all_raters, "results/figures/sensitivity_all_raters/pred_prob.png")


save_plots(p_m3_prob, "results/figures/test_plot.png")

# save_plots(p_rater_sex_prob, "results/figures/age_interaction.png")
# save_plots(p_m5_prob, "results/figures/m5_pred_prob.png")
