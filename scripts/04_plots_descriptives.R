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
data_father = readRDS("data/processed/02_data_father_clean.rds")
data_mother = readRDS("data/processed/02_data_mother_clean.rds")
data_self = readRDS("data/processed/02_data_ysr_clean.rds")
data_teacher = readRDS("data/processed/02_data_teacher_clean.rds")

# colors to use for the sexes
colors = c("Male" = "#00C07B", "Female" = "#FFBB09")
data_long$rater_type <- factor(data_long$rater_type, 
                  levels = c("m12", "v12", "t12", "ysr14"), 
                  labels = c("Mother", "Father", "Teacher", "Self"))
data_long$sex <- factor(data_long$sex, 
                 levels = c("MALE", "FEMALE"), 
                 labels = c("Male", "Female"))



       
# ridgeline plot (histogram style)
ggplot(data_long, aes(x = autism_score, y = rater_type, fill = sex)) +
  geom_density_ridges(
    stat = "binline", 
    binwidth = 1,        # Use a fixed width (e.g., 1 unit per bin)
    # boundary = 0,       # Force bins to start at 0.5, not center on 0
    scale = 0.95, 
    alpha = 0.7,
    draw_baseline = TRUE
  ) +
  # These lines will now align perfectly with the bin edges
  geom_vline(xintercept = c(0.5, 3.5), linetype = "dashed", color = "red") +
  scale_x_continuous(
    breaks = seq(0, 20, by = 2), # Explicitly define breaks to match bin logic
    expand = expansion(mult = c(-.04, 0)) 
  ) +
  geom_text(
    data = data.frame(x = c(0, 2, 4.5), y = 0.95, label = c("No", "Low", "High")),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE, # Important: prevents text from trying to use 'fill' or 'y' from main aes
    color = "red",
    size = 4
  ) +
  scale_y_discrete(expand = expansion(add = c(0.1, 0.4))) +
  labs(
    title = "Distribution of Autism Scores by Rater Type",
    subtitle = "Red lines indicate cut-offs for ordinal categories",
    x = "Autism Score (continuous)",
    y = "Rater Type"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    plot.background = element_rect(fill = "white", color = NA)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)

ggsave("results/figures/autism_score_ridgeline.png", width = 8, height = 10, dpi=600)

# stacked bar plot of ordinal autism score  
# reorder the levels of the ordinal autism score variable
data_long = data_long %>% 
    mutate(
      autism_score_ordinal = factor(
        autism_score_ordinal,
        levels = c("high", "mild", "no"),  # define order from bottom to top
        ordered = TRUE
      )
    ) 

ggplot(
  data_long,
  aes(x = sex, fill = autism_score_ordinal)
) +
  geom_bar(position = "fill") +
  facet_wrap(~ rater_type) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Ordinal Autism Score by Rater Type and Sex",
    subtitle = "Proportion in No / Medium / High categories",
    y = "Proportion",
    fill = "Autism score"
  ) +
  theme(text = element_text(size = 20))

ggsave("results/figures/autism_score_ordinal_barplot.png", width = 8, height = 6, dpi=600)

# --- boxplots of genotype data ---

ggplot(data_long, aes(x = sex, y = P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1)) +
  geom_boxplot() +
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Genotype by Sex",
       subtitle = "Comparison of Female and Male Ratings")

ggsave("results/figures/pgs_sex_boxplot.png", width = 6, height = 6, dpi=600)

# within subject comparison of raters

data_long$sub_cat = paste(data_long$FISNumber, data_long$sex)

ggplot(data_long, aes(x=rater_type, y = autism_score, group=sub_cat, color=sex)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.2) +
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Autism Scores by Rater Type",
       x = "Rater Type",
       y = "Autism Score")
# this plot is completely chaotic and unusable 


# box plot separated on sex with mean line instead of median
ggplot(data_long, aes(x = rater_type, y = autism_score, fill = sex)) +
  geom_boxplot(position = position_dodge(0.8), fatten = NULL, width = 0.7) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., group = sex),
               position = position_dodge(0.8),
               width = 0.75, size = 1, linetype = "solid") +
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Autism Score by Rater Type and Sex",
       x = "Rater Type",
       y = "Autism Score")

