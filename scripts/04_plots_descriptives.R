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
  stat_compare_means(method = "wilcox.test",
                     bracket.size = 0.7,
                     size= 7,
                     label = "p.signif",      # Use stars: *, **, ***
                     comparisons = list(c("FEMALE", "MALE")),  # Adjust to your factor levels
                     hide.ns = TRUE)    +      # Hide non-significant comparisons
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Autism Score by Rater Type and Sex",
       subtitle = "Comparison of Female and Male Ratings")


# --- boxplots of genotype  data ---

ggplot(data_long, aes(x = sex, y = P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1)) +
  geom_boxplot() +
  stat_compare_means(method = "t.test", 
                     bracket.size = 0.7,
                     size= 7,
                     label = "p.signif",      # Use stars: *, **, ***
                     comparisons = list(c("FEMALE", "MALE"))  # Adjust to your factor levels
                     )    +
  theme(text = element_text(size = 20)) +   # larger font size
  labs(title = "Genotype by Sex",
       subtitle = "Comparison of Female and Male Ratings")


# ordinalize data with 3 thresholds
data_long_ord = data_long %>%
  mutate(autism_score_ord = cut(autism_score, 
                                breaks = c(-Inf, 1, 4, Inf), 
                                labels = c("0", "1-4", "5+"),
                                right = FALSE))


table(data_long_ord$autism_score_ord)



# are PGS and PCs correlated?
lm(PGS ~ PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG, data = data_long) %>%
  summary()

# are PCs and autism score correlated?
lm(autism_score ~ PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG, data = data_long) %>%
  summary()

# PGS overlap with PCs in within-family PGS
pgs_within_pc <- data_long %>% group_by(FamilyNumber) %>%
  mutate(pgs_within = PGS - mean(PGS)) %>% ungroup()
lm(pgs_within ~ PC1_1KG + PC2_1KG + PC3_1KG + PC4_1KG + PC5_1KG + PC6_1KG + PC7_1KG + PC8_1KG + PC9_1KG + PC10_1KG, data = pgs_within_pc) %>% 
  summary()

lm(autism_score ~ pgs_within, data = pgs_within_pc) %>%
  summary()

# checking residuals
lm(autism_score ~ PGS , data = data_long) %>%
  residuals() %>%
  hist()
