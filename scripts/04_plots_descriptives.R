rm(list = ls(all = TRUE))
gc()

library(ggplot2)
library(ggridges)
library(ggpubr)
library(dplyr)
library(tidyr)
library(scales)

data = readRDS("data/processed/02_full_dataset_clean.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")
data_long_ysr12 = readRDS("data/processed/02_full_dataset_long_self_age_12.rds")
data_combined = bind_rows(data_long, data_long_ysr12)

# colors to use for the sexes
colors = c("Male" = "#00C07B", "Female" = "#FFBB09")

# --- PLOTTING FIGURES ---
   
# --- boxplots of genotype data ---
ggplot(data_long, aes(x = sex, y = PGS)) +
  geom_boxplot() +
  theme(text = element_text(size = 20)) +
  labs(title = "Mean autism PGS per sex") +
  ylab("autism PGS")

ggsave("results/figures/pgs_sex_boxplot.png", width = 6, height = 6, dpi=600)


# box plot separated on sex with mean line instead of median
ggplot(data_long, aes(x = rater, y = autism_score, fill = sex)) +
  geom_boxplot(position = position_dodge(0.8), fatten = NULL, width = 0.7) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., group = sex),
               position = position_dodge(0.8),
               width = 0.75, size = 1, linetype = "solid") +
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Autism Score by Rater Type and Sex",
       x = "Rater Type",
       y = "Autism Score")


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
  geom_vline(xintercept = c(0.5, 3.5), linetype = "dashed", color = "red", size = 0.6) +
    geom_text(
    data = data.frame(
      x = c(0, 2, 4.5), 
      y = -1, # place at the bottom
      label = c("No", "Low", "High"), 
      rater = factor("Self") # only at the bottom one
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    color = "red",
    size = 6,
    vjust = 1.3 # Adjusts it slightly down from the top border
  ) +
  scale_x_continuous(
    breaks = seq(0, 20, by = 1),
    minor_breaks = NULL,
    expand = expansion(mult = c(-.003, 0))
  ) +
  scale_y_continuous(
    breaks = function(x) seq(0, ceiling(max(x)), by = 200)) +
  coord_cartesian(ylim = c(-150, NA), clip = "off") +
  labs(
    title = "Distribution of Autism Scores by Rater Type",
    x = "Autism Score (continuous)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.text.y = element_text(size = 14, color = "grey30"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    strip.text.y = element_text(size = 18, angle = 0),

    
    # legend overlapping plot area
    legend.position = "inside",
    legend.justification = c(0.9, 0.9),
    legend.background = element_rect(fill = scales::alpha("white", 0.6), color = NA),
    
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_fill_manual(values = colors)

ggsave("results/figures/autism_score_histogram.png", width = 8, height = 10, dpi=600)


histogram_plots = function(data, var, bins) {
  # date_of_assessment histogram separated per rater
  ggplot(data, aes(x = {{var}}, fill = sex)) +
    geom_histogram(
      binwidth = bins,
      alpha = 0.7,
      position = "identity",
      color = "white"
    ) +
    facet_grid(rater ~ ., scales = "free_y", space = "free_y") +  # less whitespace
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      axis.text.y = element_text(size = 14, color = "grey30"),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      strip.text.y = element_text(size = 18, angle = 0),

      legend.position = "inside",
      legend.justification = c(0.9, 0.2),
      legend.background = element_rect(fill = scales::alpha("white", 0.6), color = NA),
      
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_fill_manual(values = colors)
}

date_plot = histogram_plots(data_long, `date_of_assessment`, bins=1)
age_plot = histogram_plots(data_long, `age`, bins=0.1)
date_12_plot = histogram_plots(data_combined, `date_of_assessment`, bins=1)
age_12_plot = histogram_plots(data_combined, `age`, bins=0.1)

ggsave(plot=date_plot, "results/figures/date_of_assessment_histogram.png", width = 8, height = 10, dpi=600)
ggsave(plot=age_plot, "results/figures/age_histogram.png", width = 8, height = 10, dpi=600)
ggsave(plot=age_12_plot, "results/figures/age_12_histogram.png", width = 8, height = 10, dpi=600)
ggsave(plot=date_12_plot, "results/figures/date_12_histogram.png", width = 8, height = 10, dpi=600)
