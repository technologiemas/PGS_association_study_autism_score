# PGS slopes per rater x sex from the LINEAR (lmer) counterpart of M4.
# Same figure as 08b_visualization_latent.R, but on the raw sum-score scale, so
# the y axis and the slopes are in score points instead of latent SD units.
# Model is fitted and saved at the bottom of 05_modeling.R.

rm(list = ls(all = TRUE))
gc()

# --- LIBRARIES ---
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)

source("scripts/_10_functions.R")  # sex_colours, theme_pgs()

fit_m4_lmer = readRDS("results/models/fit_m4_lmer.rds")

# Satterthwaite/KR degrees of freedom are unusable at this N (emmeans would
# refuse or take very long), and with ~30k rows the z and t intervals are the
# same to plotting precision. This also makes the CI columns asymp.LCL/asymp.UCL,
# exactly as in the latent figure.
emm_options(lmer.df = "asymptotic")

pgs_range = seq(-3, 3, 0.1)

# NB: for emmeans the grid variable must appear in specs, or it averages over `at`
linear_df = emmeans(fit_m4_lmer, ~ sex_effect * rater * PGS_scaled,
                    at = list(PGS_scaled = pgs_range)) %>%
  as.data.frame()

# --- SIGNIFICANCE --------------------------------------------------------------
# Same convention as the latent and age/date trend figures:
#   a star at the end of a line   - that PGS slope differs from zero
#   a star at the top of a panel  - the male and female slopes differ in that rater
# FDR-corrected within each family of tests (one slope per rater x sex, one sex
# contrast per rater). sig_star() lives in _10_functions.R

slopes = emtrends(fit_m4_lmer, ~ sex_effect * rater, var = "PGS_scaled")

slope_stars = test(slopes, adjust = "none") %>%
  calc_fdr_p() %>%
  transmute(rater, sex_effect, star = sig_star(p_fdr))

sex_contrast_stars = contrast(slopes, by = "rater", method = "pairwise", adjust = "none") %>%
  calc_fdr_p() %>%
  transmute(rater, star = sig_star(p_fdr))

# one star per line, just past its own end, in that sex's colour
ends = linear_df %>%
  group_by(rater, sex_effect) %>%
  slice_max(PGS_scaled, n = 1) %>%
  ungroup() %>%
  left_join(slope_stars, by = c("rater", "sex_effect")) %>%
  filter(star != "")

# one star per panel, centred, for the male-female slope contrast
sex_stars = sex_contrast_stars %>%
  filter(star != "") %>%
  mutate(PGS_scaled = mean(range(linear_df$PGS_scaled)),
         y = max(linear_df$asymp.UCL))

p_linear = ggplot(linear_df, aes(PGS_scaled, emmean, colour = sex_effect, fill = sex_effect)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.15, colour = NA) +
  geom_line(aes(linetype = sex_effect), linewidth = fig_linewidth) +
  geom_text(data = ends, aes(label = star), size = 1.4 * fig_base_size / .pt,
            fontface = "bold", hjust = -0.35, vjust = 0.72, show.legend = FALSE) +
  geom_text(data = sex_stars, aes(PGS_scaled, y, label = star), inherit.aes = FALSE,
            size = 1.4 * fig_base_size / .pt, fontface = "bold",
            colour = "grey25", vjust = 1) +
  facet_wrap(~ rater, nrow = 1) +
  scale_colour_manual(values = sex_colours, name = "Sex") +
  scale_fill_manual(values = sex_colours, guide = "none") +
  scale_linetype_manual(values = sex_linetypes, name = "Sex") +
  scale_x_continuous(expand = expansion(mult = 0.04)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  # clip = "off" lets the end-of-line stars sit in the gutter between panels
  # instead of being clipped; panel.spacing gives them that gutter
  coord_cartesian(clip = "off") +
  labs(x = "Autism PGS (SD)", y = "Predicted autism score\n(sum score)") +
  theme_pgs() +
  theme(panel.spacing.x = unit(16, "pt"),
        plot.margin = margin(5.5, 16, 5.5, 5.5, "pt"))

dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("results/figures/m4_three_way_linear.png", p_linear,
       width = fig_width_2col, height = 3.0, dpi = fig_dpi, bg = "white")

# --- the numbers behind the stars: slopes in score points per SD of PGS ---
cat("\n=== PGS slopes per rater x sex (sum-score points per SD) ===\n")
print(test(slopes, adjust = "none") %>% calc_fdr_p())
cat("\n=== Female - Male within each rater ===\n")
print(contrast(slopes, by = "rater", method = "pairwise", adjust = "none") %>% calc_fdr_p())
cat("\nwritten: results/figures/m4_three_way_linear.png\n")
