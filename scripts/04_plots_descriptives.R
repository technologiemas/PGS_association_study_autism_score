# These plots were written by Thomas Sollie but later refactored by Claude as I was duplicating a lot of code. The figures look good

rm(list = ls(all = TRUE))
gc()

library(ggplot2)
# library(ggridges)
library(ggpubr)
library(dplyr)
library(tidyr)
library(scales)
source("scripts/_10_functions.R")  # sex_colours, theme_pgs()

data = readRDS("data/processed/02_full_dataset_clean.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")
data_long_ysr12 = readRDS("data/processed/02_full_dataset_long_self_age_12.rds")
data_combined = bind_rows(data_long, data_long_ysr12)

# colors to use for the sexes
colors <- sex_colours

# --- PLOTTING FIGURES ---
   
# --- boxplots of genotype data ---
# NB: unlike every other figure here this one plots the raw `PGS` column rather than
# `PGS_scaled`, which puts the y axis on a 1e-07 scale, and it uses the ggplot2
# default theme rather than theme_pgs(). It also runs over `data_long`, so each
# individual is counted once per completed report. Worth settling before submission.
ggplot(data_long, aes(x = sex, y = PGS)) +
  geom_boxplot() +
  ylab("Autism PGS") +
  theme_pgs()

# one column: two boxes and nothing else, so it does not need the page width
ggsave("results/figures/pgs_sex_boxplot.png",
       width = fig_width_1col, height = 3.2, dpi = fig_dpi, bg = "white")


# Exploratory only - printed, never saved, so it keeps its on-figure title.
# box plot separated on sex with mean line instead of median
ggplot(data_long, aes(x = rater, y = autism_score, fill = sex)) +
  geom_boxplot(position = position_dodge(0.8), fatten = NULL, width = 0.7) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., group = sex),
               position = position_dodge(0.8),
               width = 0.75, size = 1, linetype = "solid") +
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Autism score by rater type and sex",
       x = "Rater type",
       y = "Autism score")


# Figure histogram of autism scores by rater type
# change order of sexes
data_long <- data_long %>%
  mutate(
    sex = factor(
      sex,
      levels = c("Female", "Male")  # set your desired order
    )
  )

ggplot(data_long, aes(x = autism_score, fill = sex)) +
  geom_histogram(
    binwidth = 1,
    alpha = 0.7,
    position = "identity",
    color = "white"
  ) +
  facet_grid(rater ~ ., scales = "free_y", space = "free_y") +  # less whitespace
  geom_vline(xintercept = c(0.5, 3.5), linetype = "dashed", color = "red", linewidth = 0.4) +
    geom_text(
    data = data.frame(
      x = c(0, 2, 5.2), 
      y = -1, # place at the bottom
      label = c("No", "Low", "High"), 
      rater = factor("Self") # only at the bottom one
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = "red",
    # geom_text size is in mm, not points: /.pt converts so these labels match the
    # theme's type rather than being sized independently of it
    size = fig_base_size / .pt - .5, # should be slightly smaller, hence -1
    vjust = 1.3 # Adjusts it slightly down from the top border
  ) +
  # every 2 rather than every 1: at 88mm and fig_base_size the two-digit labels
  # (10-18) run into each other. The bars are still one unit wide.
  scale_x_continuous(
    breaks = seq(0, 20, by = 2),
    minor_breaks = NULL,
    expand = expansion(mult = c(-.003, 0))
  ) +
  scale_y_continuous(
    breaks = function(x) seq(0, ceiling(max(x)), by = 200)) +
  coord_cartesian(ylim = c(-150, NA), clip = "off") +
  labs(
    x = "Autism score (continuous)",
    y = "Count",
    fill = "Sex"
  ) +
  # Sizes are left to theme_pgs() rather than pinned in points here: the absolute
  # 14 / 18 / 20pt values were tuned for the old 8 x 10in canvas and would swamp a
  # 88mm one. Only the non-size settings survive.
  theme_pgs() +
  theme(
    axis.text.y = element_text(colour = "grey30"),
    axis.text.x = element_text(colour = "black"),
    axis.line.x = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    strip.text.y = element_text(angle = 0),

    
    # legend overlapping plot area
    legend.position = "inside",
    legend.justification = c(0.9, 0.9),
    legend.background = element_rect(fill = scales::alpha("white", 0.6), color = NA),
    # theme_pgs() pins legend.key.width, which beats legend.key.size for the width,
    # so the width has to be overridden here as well as the height
    legend.key.height = unit(.8, "lines"),
    legend.key.width  = unit(.8, "lines"),

    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_fill_manual(values = colors)

# one column: four rater panels stacked vertically, so it is inherently portrait -
# a portrait figure run across both columns would eat most of a page
ggsave("results/figures/autism_score_histogram.png",
       width = fig_width_1col, height = 4.6, dpi = fig_dpi, bg = "white")



histogram_plots = function(data, var, bins, x_lab, legend_justification = c(0.9, 0.9)) {
  # date_of_assessment histogram separated per rater
  data = data %>%
    mutate(
      sex = factor(sex, levels = c("Female", "Male"))
    )

  ggplot(data, aes(x = {{var}}, fill = sex)) +
    geom_histogram(
      binwidth = bins,
      alpha = 0.7,
      position = "identity",
      color = "white"
    ) +
    facet_grid(rater ~ ., scales = "free_y", space = "free_y") +  # less whitespace
    labs(x = x_lab, y = "Count", fill = "Sex") +
    theme_pgs() +
    theme(
      axis.text.y = element_text(colour = "grey30"),
      axis.text.x = element_text(colour = "black"),
      axis.line.x = element_line(colour = "black"),
      axis.ticks.x = element_line(colour = "black"),
      strip.text.y = element_text(angle = 0),

      legend.position = "inside",
      legend.justification = legend_justification,
      legend.background = element_rect(fill = scales::alpha("white", 0.6), color = NA),
      legend.key.height = unit(.8, "lines"),
      legend.key.width  = unit(.8, "lines"),
      
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_fill_manual(values = colors)
}

# the "_12" variants add the self-report-at-age-12 sample; the figures are otherwise
# identical, so that qualifier now lives in the filename and the caption below
date_plot    = histogram_plots(data_long,     `date_of_assessment`, bins = 1,
                               x_lab = "Assessment date (year)", legend_justification = c(0.95, 0.25))
age_plot     = histogram_plots(data_long,     `age`,                bins = 0.1,
                               x_lab = "Age (years)")
date_12_plot = histogram_plots(data_combined, `date_of_assessment`, bins = 1,
                               x_lab = "Assessment date (year)", legend_justification = c(0.95, 0.25))
age_12_plot  = histogram_plots(data_combined, `age`,                bins = 0.1,
                               x_lab = "Age (years)")

# one column each, same portrait shape as the autism score histogram
save_1col = function (plot, file) ggsave(plot = plot, file,
  width = fig_width_1col, height = 4.6, dpi = fig_dpi, bg = "white")

save_1col(date_plot,    "results/figures/date_of_assessment_histogram.png")
save_1col(age_plot,     "results/figures/age_histogram.png")
save_1col(age_12_plot,  "results/figures/age_12_histogram.png")
save_1col(date_12_plot, "results/figures/date_12_histogram.png")


