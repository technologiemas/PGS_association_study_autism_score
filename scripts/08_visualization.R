# This script creates visualizations based on the model results

rm(list = ls(all = TRUE))
gc()

library(scales)
library(ordinal)
library(ggplot2)
library(emmeans)
library(dplyr)


colors <- c("Male" = "#0f7674", "Female" = "#D97706") # set colors for the plots

# calculate emmeans and contrasts for two way interaction
get_rater_sex_emmeans <- function(model_fit, outcome_var) {

  emm <- emmeans(
    model_fit,
    as.formula(paste("~ sex_effect *", outcome_var, "| rater")),
    mode = "prob",
    # at = list(PGS = 2), # uncomment to predict at a specific PGS value (e.g., 2 SDs above the mean). This is clinically relevant as it represents individuals with a high genetic liability for autism. If left commented, the predictions will be made at the mean PGS (PGS = 0).
    cov.reduce = mean # takes a mean of covariates for prediction, including PGS
  )

  probs <- as.data.frame(emm) %>%
    mutate(
      rater = rater,
      sex = sex_effect,
      !!outcome_var := .data[[outcome_var]]
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
      !!outcome_var := .data[[outcome_var]]
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
    labels = c("No", "Low", "High")
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

  contr_df[[outcome_var]] <- factor(
    contr_df[[outcome_var]],
    levels = c(1, 2, 3),
    labels = c("No", "Low", "High")
  )

  ggplot(
    contr_df,
    aes(
      x = diff_prob,
      y = rater
    )
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_point(aes(color = .data[[outcome_var]]), size = 3) +
    geom_errorbarh(
      aes(xmin = lower, xmax = upper, color = .data[[outcome_var]]),
      height = 0.2
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_color_manual(
        values = c("no" = "#1B9E77", "low" = "#D95F02", "high" = "#7570B3"),
        name = "Autism score level"
      ) +
    labs(
      x = "Difference in predicted probability (95% CI)",
      y = "Rater",
      subtitle = "Male − Female",
      caption = paste("Based on", model_label)
    ) +
    theme_minimal()
}


# --- three way interaction plots ---


get_three_way_emmeans <- function(model_fit, pgs_range = seq(-3, 3, 0.1), outcome_var ) {

  emm <- emmeans(
    model_fit,
    as.formula(paste("~ sex_effect *", outcome_var, "| PGS_scaled * rater")),
    mode = "prob",
    at = list(PGS_scaled = pgs_range)
  )

  as.data.frame(emm) %>%
    mutate(
      rater = rater,
      sex = sex_effect,
      !!outcome_var := .data[[outcome_var]]
    )
}

plot_three_way <- function(df, model_label, outcome_var) {

  df[[outcome_var]] <- factor(
    df[[outcome_var]],
    levels = c(1, 2, 3),
    labels = c("No", "Low", "High")
  )

  ggplot(
    df,
    aes(
      x = PGS_scaled,
      y = prob,
      color = sex,
      linetype = .data[[outcome_var]],
      group = interaction(.data[[outcome_var]], sex)
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
    scale_linetype_manual(
      values = c("No" = "dotted", "Low" = "longdash", "High" = "solid"),
      name = "Autism score level"
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "PGS (SDs)",
      y = "Predicted Probability (95% CI)",
      caption = paste("Based on", model_label)
    ) +
    theme(legend.position = "bottom")
}

plot_three_way_high_only <- function(df, model_label, outcome_var) {

  df[[outcome_var]] <- factor(
    df[[outcome_var]],
    levels = c(1, 2, 3),
    labels = c("No", "Low", "High")
  )

  df_high <- dplyr::filter(df, .data[[outcome_var]] == "High")

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

  emms_prob <- emmeans(model_fit, as.formula(paste("~ sex *", outcome_var, "| rater")), mode = "prob")
  plot_data <- as.data.frame(emms_prob)

  plot_data[[outcome_var]] <- factor(
    plot_data[[outcome_var]],
    levels = c(1, 2, 3),
    labels = c("No", "Low", "High")
  )

  # 2. Create the Stacked Bar Chart
  ggplot(plot_data, aes(x = sex, y = prob, fill = factor(.data[[outcome_var]]))) +
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
    scale_fill_brewer(palette = "Blues", name = "Autism score level") +
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
fit_m5 <- readRDS("results/models/fit_m5_clmm.rds")

fit_m3_sensitivity <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity.rds")
fit_m4_sensitivity <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity.rds")

fit_m3_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m3_clmm_sensitivity_all_raters.rds")
fit_m4_sensitivity_all_raters <- readRDS("results/models/sensitivity/fit_m4_clmm_sensitivity_all_raters.rds")

fit_ysr_12 = readRDS("results/models/sensitivity/fit_ysr_12_clmm_sensitivity.rds") # with the ysr at age 12 included

# --- create plots ---

# main models
res_m3 <- get_rater_sex_emmeans(fit_m3, outcome_var = "autism_score_ordinal")
df_m4 <- get_three_way_emmeans(fit_m4, outcome_var = "autism_score_ordinal")
res_m5 <- get_rater_sex_emmeans(fit_m5, outcome_var = "autism_score_ordinal")

p_m3_prob <- plot_pred_prob_rater_sex(res_m3$probs, "Model 3", outcome_var = "autism_score_ordinal")
p_m3_contr <- plot_pairwise_contrasts_rater_sex(res_m3$contrasts, "Model 3", outcome_var = "autism_score_ordinal")
p_m4_three_way <- plot_three_way(df_m4, "Model 4", outcome_var = "autism_score_ordinal")
p_m4_three_way_high_only <- plot_three_way_high_only(df_m4, "Model 4", outcome_var = "autism_score_ordinal")
p_m4_forest <- plot_forest_odds_ratios(fit_m4, "Model 4")

# to check if the significant sex*rater results from m3 holds in m5:
p_m5_prob <- plot_pred_prob_rater_sex(res_m5$probs, "Model 5", outcome_var = "autism_score_ordinal")


# sensitivity analyses
res_m3_sensitivity <- get_rater_sex_emmeans(fit_m3_sensitivity, outcome_var = "autism_score_ordinal_sensitivity")
df_m4_sensitivity <- get_three_way_emmeans(fit_m4_sensitivity, outcome_var = "autism_score_ordinal_sensitivity")

p_m3_prob_sensitivity <- plot_pred_prob_rater_sex(res_m3_sensitivity$probs, "Model 3", outcome_var = "autism_score_ordinal_sensitivity")
p_m3_contr_sensitivity <- plot_pairwise_contrasts_rater_sex(res_m3_sensitivity$contrasts, "Model 3", outcome_var = "autism_score_ordinal_sensitivity")
p_m4_three_way_sensitivity <- plot_three_way(df_m4_sensitivity, "Model 4", outcome_var = "autism_score_ordinal_sensitivity")
p_m4_three_way_high_only_sensitivity <- plot_three_way_high_only(df_m4_sensitivity, "Model 4", outcome_var = "autism_score_ordinal_sensitivity")
p_m4_forest_sensitivity <- plot_forest_odds_ratios(fit_m4_sensitivity, "Model 4")

# sensitivity analyses all raters present
res_m3_sensitivity_all_raters <- get_rater_sex_emmeans(fit_m3_sensitivity_all_raters, outcome_var = "autism_score_ordinal")
df_m4_sensitivity_all_raters <- get_three_way_emmeans(fit_m4_sensitivity_all_raters, outcome_var = "autism_score_ordinal")

p_m3_prob_sensitivity_all_raters <- plot_pred_prob_rater_sex(res_m3_sensitivity_all_raters$probs, "Model 3", outcome_var = "autism_score_ordinal")
p_m3_contr_sensitivity_all_raters <- plot_pairwise_contrasts_rater_sex(res_m3_sensitivity_all_raters$contrasts, "Model 3", outcome_var = "autism_score_ordinal")
p_m4_three_way_sensitivity_all_raters <- plot_three_way(df_m4_sensitivity_all_raters, "Model 4", outcome_var = "autism_score_ordinal")
p_m4_three_way_high_only_sensitivity_all_raters <- plot_three_way_high_only(df_m4_sensitivity_all_raters, "Model 4", outcome_var = "autism_score_ordinal")
p_m4_forest_sensitivity_all_raters <- plot_forest_odds_ratios(fit_m4_sensitivity_all_raters, "Model 4")


# res_m3_ysr12 <- get_rater_sex_emmeans(fit_ysr_12, outcome_var = "autism_score_ordinal") 

# p_m3_prob_ysr12 <- plot_pred_prob_rater_sex(res_m3_ysr12$probs, "Model 3", outcome_var = "autism_score_ordinal")
# p_m3_contr_ysr12 <- plot_pairwise_contrasts_rater_sex(res_m3_ysr12$contrasts, "Model 3", outcome_var = "autism_score_ordinal")

# save plots

dir.create("results/figures", showWarnings = FALSE)
dir.create("results/figures/sensitivity", showWarnings = FALSE)
dir.create("results/figures/sensitivity_all_raters", showWarnings = FALSE)


ggsave(
    "results/figures/pgs_sex_rater.png",
    p_m4_three_way,
    device = "png",
    width = 8.4, height = 11, units = "cm",
    dpi = 300, scale = 1.4
  )

ggsave(
    "results/figures/pgs_sex_rater_high_only.png",
    p_m4_three_way_high_only,
    device = "png",
    width = 8.4, height = 11, units = "cm",
    dpi = 300, scale = 1.4
  )

ggsave(
    "results/figures/rater_sex.png",
    p_m3_prob,
    device = "png",
    width = 13, height = 9, units = "cm",
    dpi = 300, scale = 1
  )

ggsave(
    "results/figures/sensitivity_all_raters/pred_prob.png",
    p_m3_prob_sensitivity_all_raters,
    device = "png",
    width = 13, height = 9, units = "cm",
    dpi = 300, scale = 1
  )

ggsave(
    "results/figures/rater_sex_m5.png",
    p_m5_prob,
    device = "png",
    width = 13, height = 9  , units = "cm",
    dpi = 300, scale = 1
  )

