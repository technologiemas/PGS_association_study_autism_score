# This script creates visualizations based on the model results

rm(list = ls(all = TRUE))
gc()

library(scales)
library(ordinal)
library(ggplot2)
library(emmeans)
library(dplyr)

source("scripts/_10_functions.R")  # sex_colours, level_colours, theme_pgs()

colors <- sex_colours

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

  # FDR within this family of tests (one male-female contrast per rater x level),
  # matching how these contrasts are reported in the results tables
  contr_df <- calc_fdr_p(contr) %>%
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

plot_pred_prob_rater_sex <- function(probs_df, outcome_var, contr_df = NULL) {

  probs_df[[outcome_var]] <- factor(
    probs_df[[outcome_var]],
    levels = c(1, 2, 3),
    labels = c("No", "Low", "High")
  )

  # One star per rater x level where the male-female difference at that level
  # differs from zero, FDR-corrected across the twelve tests. It sits above the
  # higher of the two intervals, so it reads as belonging to the pair rather than
  # to either sex. Passing contr_df = NULL leaves the figure unstarred.
  # zero-row placeholder, so an unstarred figure still draws a valid (empty) layer
  stars_df <- probs_df[0, , drop = FALSE] %>% mutate(star = character(0), y = numeric(0))
  if (!is.null(contr_df)) {
    contr_df[[outcome_var]] <- factor(
      contr_df[[outcome_var]],
      levels = c(1, 2, 3),
      labels = c("No", "Low", "High")
    )

    stars_df <- contr_df %>%
      transmute(rater, !!outcome_var := .data[[outcome_var]], star = sig_star(p_fdr)) %>%
      filter(star != "") %>%
      left_join(
        probs_df %>%
          group_by(across(all_of(c("rater", outcome_var)))) %>%
          summarise(y = max(asymp.UCL), .groups = "drop"),
        by = c("rater", outcome_var)
      )
  }

  ggplot(
    probs_df,
    aes(
      x = .data[[outcome_var]],
      y = prob,
      color = sex,
      group = sex
    )
  ) +
    # sex is encoded by colour AND point shape; the markers are enough here, so the
    # lines stay solid rather than adding a third redundant channel
    geom_line(position = position_dodge(width = 0.3), linewidth = fig_linewidth) +
    geom_point(aes(shape = sex), position = position_dodge(width = 0.3), size = 2.2) +
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      width = 0.2,
      linewidth = fig_linewidth * 0.75,
      position = position_dodge(width = 0.3)
    ) +
    geom_text(
      data = stars_df,
      aes(x = .data[[outcome_var]], y = y, label = star),
      inherit.aes = FALSE,
      size = 1.4 * fig_base_size / .pt,
      fontface = "bold",
      colour = "grey25",
      # a little clearance: at the low-probability levels the star would otherwise
      # sit on the line running into the next category
      vjust = -0.7
    ) +
    facet_grid(~ rater) +
    scale_color_manual(values = sex_colours, name = "Sex") +
    scale_shape_manual(values = sex_shapes, name = "Sex") +
    # headroom for the stars above the tallest interval
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0.05, 0.18))) +
    labs(
      y = "Predicted probability (95% CI)",
      x = "Autism score level"
    ) +
    theme_pgs()
}

plot_pairwise_contrasts_rater_sex <- function(contr_df, outcome_var) {

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
      scale_color_manual(values = level_colours, name = "Autism score level") +
    labs(
      x = "Difference in predicted probability, male − female (95% CI)",
      y = "Rater"
    ) +
    theme_pgs()
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

plot_three_way <- function(df, outcome_var) {

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
      color = .data[[outcome_var]],
      linetype = sex,
      group = interaction(.data[[outcome_var]], sex)
    )
  ) +
    geom_ribbon(
      aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = .data[[outcome_var]]),
      alpha = 0.1,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(linewidth = fig_linewidth) +
    facet_wrap(~ rater, ncol = 2) +
    scale_color_manual(values = level_colours, name = "Autism score level") +
    scale_fill_manual(values = level_colours, guide = "none") +
    scale_linetype_manual(values = sex_linetypes, name = "Sex") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "Autism PGS (SD)",
      y = "Predicted probability (95% CI)"
    ) +
    theme_pgs() +
    # wide keys so the dashed (Female) line is distinguishable from the solid one
    theme(legend.box = "vertical", legend.spacing.y = unit(2, "pt"),
          legend.key.width = unit(1.3, "cm"))
}

plot_three_way_high_only <- function(df, outcome_var) {

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
      alpha = 0.12,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(aes(linetype = sex), linewidth = fig_linewidth) +
    facet_wrap(~ rater, ncol = 2) +
    scale_color_manual(values = sex_colours, name = "Sex") +
    scale_fill_manual(values = sex_colours, guide = "none") +
    scale_linetype_manual(values = sex_linetypes, name = "Sex") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "Autism PGS (SD)",
      y = "Predicted probability (95% CI)"
    ) +
    theme_pgs() +
    theme(legend.box = "horizontal"
)
}

plot_forest_odds_ratios <- function(model_fit) {

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
      x = "Odds ratio (log scale)",
      y = NULL
    ) +
    theme_pgs()
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
    scale_fill_manual(values = level_colours, name = "Autism score level") +
    scale_y_continuous(labels = percent_format()) +
    labs(
      x = "Sex",
      y = "Predicted probability (%)"
    ) +
    theme_pgs() +
    theme(panel.grid.major.x = element_blank())
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

p_m3_prob <- plot_pred_prob_rater_sex(res_m3$probs, outcome_var = "autism_score_ordinal", contr_df = res_m3$contrasts)
p_m3_contr <- plot_pairwise_contrasts_rater_sex(res_m3$contrasts, outcome_var = "autism_score_ordinal")
p_m4_three_way <- plot_three_way(df_m4, outcome_var = "autism_score_ordinal")
p_m4_three_way_high_only <- plot_three_way_high_only(df_m4, outcome_var = "autism_score_ordinal")
p_m4_forest <- plot_forest_odds_ratios(fit_m4)

# to check if the significant sex*rater results from m3 holds in m5:
p_m5_prob <- plot_pred_prob_rater_sex(res_m5$probs, outcome_var = "autism_score_ordinal", contr_df = res_m5$contrasts)


# sensitivity analyses
res_m3_sensitivity <- get_rater_sex_emmeans(fit_m3_sensitivity, outcome_var = "autism_score_ordinal_sensitivity")
df_m4_sensitivity <- get_three_way_emmeans(fit_m4_sensitivity, outcome_var = "autism_score_ordinal_sensitivity")

p_m3_prob_sensitivity <- plot_pred_prob_rater_sex(res_m3_sensitivity$probs, outcome_var = "autism_score_ordinal_sensitivity", contr_df = res_m3_sensitivity$contrasts)
p_m3_contr_sensitivity <- plot_pairwise_contrasts_rater_sex(res_m3_sensitivity$contrasts, outcome_var = "autism_score_ordinal_sensitivity")
p_m4_three_way_sensitivity <- plot_three_way(df_m4_sensitivity, outcome_var = "autism_score_ordinal_sensitivity")
p_m4_three_way_high_only_sensitivity <- plot_three_way_high_only(df_m4_sensitivity, outcome_var = "autism_score_ordinal_sensitivity")
p_m4_forest_sensitivity <- plot_forest_odds_ratios(fit_m4_sensitivity)

# sensitivity analyses all raters present
res_m3_sensitivity_all_raters <- get_rater_sex_emmeans(fit_m3_sensitivity_all_raters, outcome_var = "autism_score_ordinal")
df_m4_sensitivity_all_raters <- get_three_way_emmeans(fit_m4_sensitivity_all_raters, outcome_var = "autism_score_ordinal")

p_m3_prob_sensitivity_all_raters <- plot_pred_prob_rater_sex(res_m3_sensitivity_all_raters$probs, outcome_var = "autism_score_ordinal", contr_df = res_m3_sensitivity_all_raters$contrasts)
p_m3_contr_sensitivity_all_raters <- plot_pairwise_contrasts_rater_sex(res_m3_sensitivity_all_raters$contrasts, outcome_var = "autism_score_ordinal")
p_m4_three_way_sensitivity_all_raters <- plot_three_way(df_m4_sensitivity_all_raters, outcome_var = "autism_score_ordinal")
p_m4_three_way_high_only_sensitivity_all_raters <- plot_three_way_high_only(df_m4_sensitivity_all_raters, outcome_var = "autism_score_ordinal")
p_m4_forest_sensitivity_all_raters <- plot_forest_odds_ratios(fit_m4_sensitivity_all_raters)


# res_m3_ysr12 <- get_rater_sex_emmeans(fit_ysr_12, outcome_var = "autism_score_ordinal") 

# p_m3_prob_ysr12 <- plot_pred_prob_rater_sex(res_m3_ysr12$probs, outcome_var = "autism_score_ordinal")
# p_m3_contr_ysr12 <- plot_pairwise_contrasts_rater_sex(res_m3_ysr12$contrasts, outcome_var = "autism_score_ordinal")

# save plots

dir.create("results/figures", showWarnings = FALSE)
dir.create("results/figures/sensitivity", showWarnings = FALSE)
dir.create("results/figures/sensitivity_all_raters", showWarnings = FALSE)


# All five are two-column figures: each lays four rater panels out at once, either
# across a row or as a 2 x 2 grid, which does not survive being squeezed into 88 mm.
# No `scale` argument anywhere - scaling a saved figure is what makes the type sizes
# drift apart between figures.

ggsave(
    "results/figures/pgs_sex_rater.png",
    p_m4_three_way,
    device = "png",
    width = fig_width_2col, height = 5.6,
    dpi = fig_dpi, bg = "white"
  )

ggsave(
    "results/figures/pgs_sex_rater_high_only.png",
    p_m4_three_way_high_only,
    device = "png",
    width = fig_width_2col, height = 5.2,
    dpi = fig_dpi, bg = "white"
  )

ggsave(
    "results/figures/rater_sex.png",
    p_m3_prob,
    device = "png",
    width = fig_width_2col, height = 3.2,
    dpi = fig_dpi, bg = "white"
  )

ggsave(
    "results/figures/sensitivity_all_raters/pred_prob.png",
    p_m3_prob_sensitivity_all_raters,
    device = "png",
    width = fig_width_2col, height = 3.2,
    dpi = fig_dpi, bg = "white"
  )

ggsave(
    "results/figures/rater_sex_m5.png",
    p_m5_prob,
    device = "png",
    width = fig_width_2col, height = 3.2,
    dpi = fig_dpi, bg = "white"
  )


