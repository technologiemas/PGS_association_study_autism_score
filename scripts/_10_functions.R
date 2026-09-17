
calc_fdr_p <- function(emm_obj, method = "fdr") {
  p_vals <- summary(emm_obj)$p.value

  out <- as.data.frame(emm_obj) %>%
    rename(any_of(c(
      "p_unadjusted"   = "p.value",
      "p_unadjusted"   = "p-value",
      "p_unadjusted"   = "p_value"
    )))

  # out$p_fdr <- p.adjust(out$p_unadjusted, method = method)
  out$p_fdr <- p.adjust(p_vals, method = method)

  out
}

# a p-value reduced to the star convention used across the figures
sig_star <- function(p, alpha = 0.05) ifelse(!is.na(p) & p < alpha, "*", "")

rename_prob_columns <- function(emm_obj) {
  out <- as.data.frame(emm_obj)

  out <- out %>%
    mutate(
      `Probability Difference` = estimate,
      SE = SE,
      `Prob 95% CI Lower` = estimate - 1.96 * SE,
      `Prob 95% CI Upper` = estimate + 1.96 * SE
    ) %>%
    select(-estimate, -SE) # remove the original 'estimate' column as it's now represented as 'Probability'

  out
}

rename_columns <- function(emm_obj) {
  out <- as.data.frame(emm_obj)

  target_col <- intersect(c("estimate", "emmean", "PGS_scaled.trend", "age_centered.trend", "date_of_assessment_centered.trend"), names(out))[1]

  if (!is.na(target_col)) { # else it is a log-odds estimate
    out <- out %>%
      mutate(
        `Odds Ratio`      = exp(.data[[target_col]]),
        `OR 95% CI Lower` = exp(.data[[target_col]] - 1.96 * SE),
        `OR 95% CI Upper` = exp(.data[[target_col]] + 1.96 * SE)
      )
  }

  out <- out %>%
    rename(any_of(c(
      "p-value"                            = "p.value",
      "Estimated Probability"              = "prob",
      "Estimate difference (log-odds)"     = "estimate",
      "Estimated Marginal Mean (log-odds)" = "emmean",
      "Slope of PGS (scaled) in log-odds"  = "PGS_scaled.trend"
    ))) %>%
    select(-contains("asymp.LCL"), -contains("asymp.UCL"))

  return(out)
}

# The following code was AI generated - it seems to work perfectly fine
# emmeans takes the names of the ordinal outcome levels from dimnames(tJac), which clmm
# objects do not store, so mode = "prob" falls back to numbering them 1, 2, 3. Restoring the
# threshold names ("no|low", "low|high") makes the levels print as no / low / high instead.
restore_ylevel_names <- function(fit_model) {
  if (is.null(dimnames(fit_model$tJac)[[1]]) &&
      length(fit_model$alpha) == nrow(fit_model$tJac)) {
    dimnames(fit_model$tJac) <- list(names(fit_model$alpha), NULL)
  }

  fit_model
}


# The following is from a Claude refactor of 04_plots_descriptives and 08_visualization as there was a lot of repeated code
# The figures are identical to the originals

# --- SHARED FIGURE STYLING -----------------------------------------------------
# One theme and one palette for every figure in the project, so the figure set is
# consistent without per-script drift.
#
# SEX IS ENCODED TWICE - colour PLUS point shape (figures with markers) or linetype
# (line-only figures). This is the part that matters. Printing or photocopying the
# article collapses colour to luminance, and no two-hue palette solves that on its
# own: greyscale separation needs one light and one dark hue, and the light one then
# falls below 3:1 contrast against a white page. Carrying identity on a second,
# non-colour channel also covers colour-vision deficiency and forced-colours mode.
# Because that channel is there, the palette only has to be good, not heroic.
#
# PALETTE (teal / orange) - measured with the data-viz validator:
#   CVD separation      dE 14.7 (protan), 30.4 (tritan)   PASS
#   Normal-vision floor dE 27.2                           PASS
#   Contrast vs white   5.29 and 3.10, both >= 3:1        PASS
#   Greyscale           1.71:1, i.e. grey 106 vs 144 of 255 - a visible but modest
#                       gap, which is exactly why shape/linetype carry identity.
#   Chroma floor        teal 0.085 vs a 0.10 floor        FAIL (accepted)
#     This one is a property of the hue, not of the choice: the most saturated teal
#     that still reads as teal (#008b85) only reaches 0.099. The check flags "might
#     read as grey"; with a second encoding channel present it is not a problem.
#
# The Male/Female assignment is arbitrary - swap the two hex values to reverse it.
#
# LEVELS. Autism score level is ORDERED (No < Low < High), so it gets a single-hue
# sequential ramp light -> dark. A luminance ramp survives greyscale by construction.

sex_colours   <- c(Male = "#0f7674", Female = "#D97706")
sex_linetypes <- c(Male = "solid",   Female = "22")
sex_shapes    <- c(Male = 16,        Female = 17)

level_colours <- c(No = "#86b6ef", Low = "#2a78d6", High = "#104281")

# --- FIGURE SIZING -------------------------------------------------------------
# Two canvas widths, matching the one- and two-column widths of a typical journal
# page. Every figure is saved AT ITS FINAL PRINT SIZE and is not rescaled by the
# journal afterwards, which is what keeps the type consistent: a point is a point,
# so a one-column and a two-column figure set in fig_base_size have the same font
# on the page. Rescaling a figure after saving is what makes font sizes drift.
#
# Check these against the target journal's author guidelines before submitting;
# 88 / 183 mm is the common Nature and Elsevier pair, but it does vary.
fig_width_1col = 3.46   # 88 mm
fig_width_2col = 7.20   # 183 mm
fig_base_size  = 10     # pt on the printed page; journals typically accept 7-10
fig_linewidth  = 0.8    # pt, for data lines - thin defaults disappear in print
fig_dpi        = 600    # for line art at final size; 300 is the floor, 600 is safe

theme_pgs <- function(base_size = fig_base_size) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor     = ggplot2::element_blank(),
      panel.grid.major     = ggplot2::element_line(colour = "grey92"),
      strip.text           = ggplot2::element_text(face = "bold"),
      plot.title           = ggplot2::element_text(face = "bold"),
      plot.subtitle        = ggplot2::element_text(colour = "grey35"),
      plot.caption         = ggplot2::element_text(colour = "grey35"),
      axis.title           = ggplot2::element_text(colour = "grey35"),
      legend.position      = "top",
      legend.justification = "left",
      legend.margin        = ggplot2::margin(0, 0, 0, 0, "pt"),
      # scales with the type so the dashed Female line stays readable at both widths
      legend.key.width     = ggplot2::unit(0.1 * base_size, "cm")
    )
}
