# This script was AI generated but based on previously written code by me 
# in script 08_visualization.R 
# the code seems correct and the figures look good

rm(list = ls(all = TRUE))
gc()

# --- LIBRARIES ---
library(dplyr)
library(ordinal)
library(emmeans)
library(ggplot2)

source("scripts/_10_functions.R")  # sex_colours, theme_pgs()

# --- LOAD ---
data_long = readRDS("data/processed/02_full_dataset_long.rds")
fit_age_sex_rater = readRDS("results/models/fit_age_sex_rater_clmm.rds")

# per-rater centring constants and observed ranges
rater_info = data_long %>%
  group_by(rater) %>%
  summarise(mean_age = mean(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE), max_age = max(age, na.rm = TRUE),
            mean_date = mean(date_of_assessment, na.rm = TRUE),
            min_date = min(date_of_assessment, na.rm = TRUE),
            max_date = max(date_of_assessment, na.rm = TRUE), .groups = "drop")

# --- BUILD THE PREDICTION GRID -------------------------------------------------
# One sequence per rater across its own observed range, expressed on the centred
# scale the model was fitted on, then matched back after emmeans.
build_trend_data = function (var, mean_col, min_col, max_col, n = 60) {
  grid = rater_info %>%
    rowwise() %>%
    reframe(rater = rater,
            observed = seq(.data[[min_col]], .data[[max_col]], length.out = n),
            centred = round(observed - .data[[mean_col]], 6))

  other = setdiff(c("age_centered", "date_of_assessment_centered"), var)
  at_list = setNames(list(sort(unique(grid$centred)), 0), c(var, other))

  # NB: for emmeans the grid variable MUST appear in specs, otherwise emmeans
  # averages over the `at` values instead of keeping them as a dimension.
  specs = as.formula(paste("~ sex_effect * rater *", var))
  emm = emmeans(fit_age_sex_rater, specs, at = at_list, mode = "latent") %>% # on the latent scale
    as.data.frame() %>%
    mutate(centred = round(.data[[var]], 6))

  inner_join(emm, grid, by = c("rater", "centred"))
}

# --- SIGNIFICANCE --------------------------------------------------------------
# Same tests as in 9b_age_date_post_hoc_sensitivity.R, FDR-corrected within each
# family of tests (one slope per rater x sex, one sex contrast per rater), reduced
# to a star per cell:
#   slopes    - is this rater x sex trend different from zero?
#   contrasts - do the male and female trends differ within this rater?
# sig_star() lives in _10_functions.R

# p_col = "p_unadjusted" switches the stars to uncorrected p-values
trend_significance = function (var, p_col = "p_fdr") {
  emt = emtrends(fit_age_sex_rater, ~ sex_effect * rater, var = var)

  slopes = test(emt, adjust = "none") %>%
    calc_fdr_p() %>%
    transmute(rater, sex_effect, star = sig_star(.data[[p_col]]))

  sex_contrasts = contrast(emt, by = "rater", method = "pairwise", adjust = "none") %>%
    calc_fdr_p() %>%
    transmute(rater, star = sig_star(.data[[p_col]]))

  list(slopes = slopes, sex_contrasts = sex_contrasts)
}

# --- PLOT ----------------------------------------------------------------------
trend_plot = function (df, x_lab, x_breaks, sig) {
  # Slope-vs-zero star, one per line, sitting just past the end of its own line in
  # the right-hand margin. The Male/Female text labels that used to live here were
  # dropped: the legend already encodes sex twice (colour AND linetype), so the
  # labels were redundant, and being wide they collided with each other and with
  # the other sex's line wherever the two trends converge. A star is narrow enough
  # to never collide, and inherits the sex colour, so it stays identifiable.
  ends = df %>%
    group_by(rater, sex_effect) %>%
    slice_max(observed, n = 1) %>%
    ungroup() %>%
    left_join(sig$slopes, by = c("rater", "sex_effect")) %>%
    filter(star != "")

  # one star per panel, centred on the rater's own x range, for the male-female
  # slope contrast; drawn at the top of the shared y scale
  sex_stars = df %>%
    group_by(rater) %>%
    summarise(observed = mean(range(observed)), .groups = "drop") %>%
    left_join(sig$sex_contrasts, by = "rater") %>%
    filter(star != "") %>%
    mutate(y = max(df$asymp.UCL))

  ggplot(df, aes(observed, emmean, colour = sex_effect, fill = sex_effect)) +
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.15, colour = NA) +
    geom_line(aes(linetype = sex_effect), linewidth = fig_linewidth) +
    # 1.4x the body type: the stars are the only mark carrying the test results, so
    # they are deliberately larger, but they track fig_base_size rather than being
    # pinned in mm and drifting when the canvas changes
    geom_text(data = ends, aes(label = star), size = 1.4 * fig_base_size / .pt,
              fontface = "bold", hjust = -0.35, vjust = 0.72, show.legend = FALSE) +
    geom_text(data = sex_stars, aes(observed, y, label = star), inherit.aes = FALSE,
              size = 1.4 * fig_base_size / .pt, fontface = "bold",
              colour = "grey25", vjust = 1) +
    # space = "free_x" makes one year the same width in every panel, so the
    # slopes are visually comparable and the short self-report window is honest
    # about covering a third of the span.
    facet_grid(~ rater, scales = "free_x", space = "free_x") +
    scale_colour_manual(values = sex_colours, name = "Sex") +
    scale_fill_manual(values = sex_colours, guide = "none") +
    scale_linetype_manual(values = sex_linetypes, name = "Sex") +
    # fixed-interval breaks: the narrow self panel gets few ticks, not crammed ones
    scale_x_continuous(breaks = x_breaks, expand = expansion(mult = 0.04)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
    # clip = "off" lets the end-of-line stars sit in the gutter between panels
    # instead of being clipped. Widening panel.spacing gives them that gutter, and
    # this works for any x range - unlike padding the x scale, which would have to
    # be retuned per figure and is worst in the narrow Self panel.
    coord_cartesian(clip = "off") +
    # No title, subtitle or caption on the figure itself: that text belongs in the
    # manuscript's figure caption, where it is typeset and copy-edited with the rest
    # of the article. The strings live at the bottom of this script instead.
    labs(x = x_lab, y = "Predicted autism score\n(latent scale)") +
    theme_pgs() +
    theme(panel.spacing.x = unit(16, "pt"),
          plot.margin = margin(5.5, 16, 5.5, 5.5, "pt"))
}

dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)

date_df = build_trend_data("date_of_assessment_centered", "mean_date", "min_date", "max_date")
p_date = trend_plot(date_df, "Assessment date (year)",
  x_breaks = seq(1995, 2025, by = 5),
  sig = trend_significance("date_of_assessment_centered"))
ggsave("results/figures/date_sex_rater_trends.png", p_date,
       width = fig_width_2col, height = 3.0, dpi = fig_dpi, bg = "white")

age_df = build_trend_data("age_centered", "mean_age", "min_age", "max_age")
p_age = trend_plot(age_df, "Age (years)",
  x_breaks = seq(11, 17, by = 1),
  sig = trend_significance("age_centered"))
ggsave("results/figures/age_sex_rater_trends.png", p_age,
       width = fig_width_2col, height = 3.0, dpi = fig_dpi, bg = "white")
