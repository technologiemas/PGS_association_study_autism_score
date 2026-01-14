rm(list = ls(all = TRUE))
gc()

library(ggplot2)
library(ggpubr)


data = readRDS("data/processed/02_full_dataset_clean.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")
data_father = readRDS("data/processed/02_data_father_clean.rds")
data_mother = readRDS("data/processed/02_data_mother_clean.rds")
data_self = readRDS("data/processed/02_data_ysr_clean.rds")
data_teacher = readRDS("data/processed/02_data_teacher_clean.rds")


# plot the distribution of the phenotype data
ggplot(data, aes(x = m12_aut_sum)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of mother aut sum", x = "m12_aut_sum", y = "Count") 


# --- boxplots of phenotype data ---

# box plot of all raters separated on sex
ggplot(data_long, aes(x = sex, y = autism_score)) +
  geom_boxplot(fatten = NULL) +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid") +
  facet_wrap(~ rater_type) +
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Autism Score by Rater Type and Sex",
       subtitle = "Comparison of Female and Male Ratings")

# stacked bar plot of ordinal autism score
ggplot(
  data_long %>% 
    mutate(
      autism_score_ordinal = factor(
        autism_score_ordinal,
        levels = c("high", "mild", "no"),  # define order from bottom to top
        ordered = TRUE
      )
    ) %>%
    filter(!is.na(sex), !is.na(autism_score_ordinal)),  # remove rows with NA
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

# --- boxplots of genotype  data ---

ggplot(data_long, aes(x = sex, y = P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1)) +
  geom_boxplot() +
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Genotype by Sex",
       subtitle = "Comparison of Female and Male Ratings")

