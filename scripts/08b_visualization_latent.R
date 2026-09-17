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

fit_m4 = readRDS("results/models/fit_m4_clmm.rds")

pgs_range = seq(-3, 3, 0.1)

# NB: for emmeans the grid variable must appear in specs, or it averages over `at`
latent_df = emmeans(fit_m4, ~ sex_effect * rater * PGS_scaled,
                    at = list(PGS_scaled = pgs_range), mode = "latent") %>%
  as.data.frame()

# --- SIGNIFICANCE --------------------------------------------------------------
# Same convention as the age/date trend figures in 9c_age_date_trend_figure.R:
#   a star at the end of a line   - that PGS slope differs from zero
#   a star at the top of a panel  - the male and female slopes differ in that rater
# FDR-corrected within each family of tests (one slope per rater x sex, one sex
# contrast per rater), matching how these are reported in the results.
# sig_star() lives in _10_functions.R

slopes = emtrends(fit_m4, ~ sex_effect * rater, var = "PGS_scaled", mode = "latent")

slope_stars = test(slopes, adjust = "none") %>%
  calc_fdr_p() %>%
  transmute(rater, sex_effect, star = sig_star(p_fdr))

sex_contrast_stars = contrast(slopes, by = "rater", method = "pairwise", adjust = "none") %>%
  calc_fdr_p() %>%
  transmute(rater, star = sig_star(p_fdr))

# one star per line, just past its own end, in that sex's colour
ends = latent_df %>%
  group_by(rater, sex_effect) %>%
  slice_max(PGS_scaled, n = 1) %>%
  ungroup() %>%
  left_join(slope_stars, by = c("rater", "sex_effect")) %>%
  filter(star != "")

# one star per panel, centred, for the male-female slope contrast
sex_stars = sex_contrast_stars %>%
  filter(star != "") %>%
  mutate(PGS_scaled = mean(range(latent_df$PGS_scaled)),
         y = max(latent_df$asymp.UCL))

p_latent = ggplot(latent_df, aes(PGS_scaled, emmean, colour = sex_effect, fill = sex_effect)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.15, colour = NA) +
  geom_line(aes(linetype = sex_effect), linewidth = fig_linewidth) +
  # No Male/Female labels at the line ends: the legend already encodes sex twice
  # (colour AND linetype), and at this width the labels sat on top of the lines.
  # The stars are narrow enough to sit there without colliding.
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
  # No title or subtitle on the figure itself: that text belongs in the manuscript's
  # figure caption, which is built at the bottom of this script.
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.10))) +
  # clip = "off" lets the end-of-line stars sit in the gutter between panels
  # instead of being clipped; panel.spacing gives them that gutter
  coord_cartesian(clip = "off") +
  labs(x = "Autism PGS (SD)", y = "Predicted autism score\n(latent scale)") +
  theme_pgs() +
  theme(panel.spacing.x = unit(16, "pt"),
        plot.margin = margin(5.5, 16, 5.5, 5.5, "pt"))

dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("results/figures/m4_three_way_latent.png", p_latent,
       width = fig_width_2col, height = 3.0, dpi = fig_dpi, bg = "white")

# --- the numbers behind the stars: latent slopes and their contrasts ---
cat("\n=== latent PGS slopes per rater x sex ===\n")
print(test(slopes, adjust = "none") %>% calc_fdr_p())
cat("\n=== Female - Male within each rater ===\n")
print(contrast(slopes, by = "rater", method = "pairwise", adjust = "none") %>% calc_fdr_p())
cat("\nwritten: results/figures/m4_three_way_latent.png\n")


# --- M3: sex x rater on the latent scale ---------------------------------------
# The probability-scale version of this (results/figures/rater_sex.png, built in
# 08_visualization.R) splits each rater into three outcome levels. On the latent
# scale each rater x sex is a single number, so the male-female gap can be read
# off directly and compared across raters.

fit_m3 = readRDS("results/models/fit_m3_clmm.rds")

# at mean PGS, averaged over the other covariates (PLATFORM, PCs), which is what
# makes the four gaps comparable to each other
emm_m3 = emmeans(fit_m3, ~ sex_effect * rater, mode = "latent", cov.reduce = mean)
m3_df = as.data.frame(emm_m3)

# one star per rater where the male-female gap differs from zero, FDR-corrected
# across the four raters - same convention as the figure above
m3_sex_stars = contrast(emm_m3, by = "rater", method = "pairwise", adjust = "none") %>%
  calc_fdr_p() %>%
  transmute(rater, star = sig_star(p_fdr)) %>%
  filter(star != "") %>%
  left_join(m3_df %>% group_by(rater) %>% summarise(y = max(asymp.UCL), .groups = "drop"),
            by = "rater")

# sex is encoded by colour AND point shape, as everywhere else in the figure set
m3_dodge = position_dodge(width = 0.5)

p_m3_latent = ggplot(m3_df, aes(rater, emmean, colour = sex_effect)) +
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL, shape = sex_effect),
                  position = m3_dodge, size = 0.45, linewidth = fig_linewidth) +
  geom_text(data = m3_sex_stars, aes(rater, y, label = star), inherit.aes = FALSE,
            size = 1.4 * fig_base_size / .pt, fontface = "bold",
            colour = "grey25", vjust = -0.4) +
  scale_colour_manual(values = sex_colours, name = "Sex") +
  scale_shape_manual(values = sex_shapes, name = "Sex") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  # No title or subtitle here either; the caption belongs in the manuscript.
  labs(x = NULL, y = "Predicted autism score\n(latent scale)") +
  theme_pgs() +
  theme(
    # the x positions are categories, not values, so vertical gridlines say nothing
    panel.grid.major.x = element_blank(),
    # the rater names are the panel labels of this figure, so they are set like the
    # facet strips of every other rater figure rather than like ordinary axis text
    axis.text.x = element_text(face = "bold", colour = "grey10", size = fig_base_size)
  )

ggsave("results/figures/m3_rater_sex_latent.png", p_m3_latent,
       width = fig_width_2col, height = 3.2, dpi = fig_dpi, bg = "white")

cat("\n=== latent means per rater x sex (M3, at mean PGS) ===\n")
print(summary(emm_m3, infer = c(TRUE, TRUE)) %>% calc_fdr_p())
cat("\n=== Male - Female within each rater (M3) ===\n")
print(contrast(emm_m3, by = "rater", method = "pairwise", adjust = "none") %>% calc_fdr_p())
cat("\nwritten: results/figures/m3_rater_sex_latent.png\n")
