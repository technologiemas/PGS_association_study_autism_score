rm(list = ls(all = TRUE))
gc()

library(marginaleffects)
library(tidybayes)
library(ggplot2)
library(dplyr)
library(bayestestR)

# Load bayesian models
fit_m4_bayesian <- readRDS("results/models/test_m4_bayes.rds")

colors = c("Male" = "#00C07B", "Female" = "#FFBB09")

# model interactions: PGS_scaled * rater_type + PGS_scaled * sex + sex * rater_type


# --- rater_type * sex ---
# Calculate the Difference (Male minus Female) for each rater
sex_diffs <- comparisons(
  fit_m4_bayesian,
  variables = "sex",        # We want to see the effect of sex
  by = "rater_type",        # ...separately for each rater
  type = "link",
  ndraws = 500
)

p_direction(sex_diffs)

rope(fit_m4_bayesian, range = c(-0.05, 0.05))

# calculate means for each sex within each rater
means_rater_sex <- predictions(
  fit_m4_bayesian,
  variables = c("rater_type", "sex"),
  type = "link",
  ndraws = 500
)
p_direction(means_rater_sex)

# save means_rater_sex
# saveRDS(means_rater_sex, "results/models/test_means_rater_sex_bayesian.rds")
# means_rater_sex = readRDS("results/models/test_means_rater_sex_bayesian.rds")

# 2. Extract the posterior draws of these
draws_diffs_rater_sex <- posterior_draws(sex_diffs)

draws_means_rater_sex <- posterior_draws(means_rater_sex)
draws_means_rater_sex <- draws_means_rater_sex[, c("draw", "sex", "rater_type")] # select only relevant columns
draws_means_rater_sex <- draws_means_rater_sex %>% # downsample for quicker plotting
  group_by(sex, rater_type) %>%
  slice_sample(n = 1000) %>%
  ungroup()

# 3a plot diffs figure
draws_diffs_rater_sex %>%
  ggplot(aes(x = draw, y = rater_type)) +
  # The ROPE gray box (Null region: -0.02 to 0.02) <- fix this
  # If the pink slab is outside this box, the sex difference is "substantial"
  annotate("rect", xmin = 0.98, xmax = 1.02, ymin = -Inf, ymax = Inf, 
           fill = "gray", alpha = 0.4) +
  
  # Reference line at 0 (No Sex Difference)
  geom_vline(xintercept = 1, color = "black", size = 0.8) +
  geom_vline(xintercept = c(0.95, 1.05), linetype = "dashed", color = "darkgray") +
  
  # The Slabs (Density)
  stat_slab(aes(fill = after_stat(cut_cdf_qi(cdf, .width = c(.95, 1)))), 
            alpha = 0.6, show.legend = FALSE) +
  stat_pointinterval(.width = c(.66, .95)) +
  
  # Colors matching your aesthetic, with different colors for the sexes
  scale_fill_manual(values = c("#E91E63", "#F8BBD0")) +
  
  # Styling
  scale_x_continuous(limits = c(0, 2)) +
  labs(
    x = "Sex Difference (Male - Female) in Odds Ratio",
    y = "Rater Type",
    title = "Posterior Distribution of Sex Differences by Rater",
    subtitle = "Values > 0 indicate Males are rated higher than Females"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(face = "bold"))

# save figure
ggsave("results/figures/bayesian/test_sex_rater_diffs.png", width = 8, height = 6)

# 3b plot means figure
draws_means_rater_sex %>%
  mutate(OR = exp(draw)) %>%
  # Ensure draw is converted to Odds Ratio if not already done
  ggplot(aes(x = OR, y = rater_type, fill = sex, color = sex)) +

    annotate("rect", xmin = 0.98, xmax = 1.02, ymin = -Inf, ymax = Inf, 
           fill = "gray", alpha = 0.4) +
  
  # Reference line at 1 (No Difference for Odds Ratios)
  geom_vline(xintercept = 1, color = "black", size = 0.8) +
  geom_vline(xintercept = c(0.95, 1.05), linetype = "dashed", color = "darkgray") +
  
  # The Slabs (Density) 
  # We use 'alpha' to create the 95% shading effect so 'fill' is free for 'sex'
  stat_slab(aes(alpha = after_stat(cut_cdf_qi(cdf, .width = c(.95, 1)))), 
            show.legend = TRUE) +
  
  # Point interval for the median and 66/95% CI
  stat_pointinterval(.width = c(.66, .95), position = position_dodge(width = 0.05)) +
  
  # Define your custom colors for Sex
  # Female = #E91E63 (Pink), Male = #3F51B5 (Blue/Purple)
  scale_fill_manual(values = c("FEMALE" = "#FFBB09", "MALE" = "#00C07B")) +
  scale_color_manual(values = c("FEMALE" = "#FFBB09", "MALE" = "#00C07B")) +
  
  # This makes the 95% tails lighter than the center
  scale_alpha_manual(values = c(0.5, 0.2), guide = "none") +
  
  # Styling
  scale_x_continuous(limits = c(0, 2)) +
  labs(
    x = "Odds Ratio for Autism Score Category",
    y = "Rater Type",
    title = "Posterior Distribution of Scores by Rater and Sex",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom"
  )


# --- rater_type * PGS ---

slopes_rater_pgs <- slopes(
  fit_m4_bayesian,
  variables = "PGS_scaled",        
  by = "rater_type",        
  type = "link"
)

# 2. Extract the posterior draws
draws_slopes_rater_pgs <- posterior_draws(slopes_rater_pgs)

# 3. Create the plot
draws_slopes_rater_pgs %>%
  # mutate(group = paste(rater_type, sex, sep = " x ")) %>%
  ggplot(aes(x = draw, y = rater_type)) +
  
  # The ROPE gray box (Null region) 
  # Note: For means, the "0" line is just a relative point, 
  # but keeping the gray box helps maintain the aesthetic of the original paper.
  annotate("rect", xmin = -0.02, xmax = 0.02, ymin = -Inf, ymax = Inf, 
           fill = "gray", alpha = 0.4) +
  
  geom_vline(xintercept = 0, color = "darkgray", linetype = "solid") +
  
  # The "Pink Slab" aesthetic
  stat_slab(aes(fill = after_stat(cut_cdf_qi(cdf, .width = c(.95, 1)))), 
            alpha = 0.6, show.legend = FALSE) +
  stat_pointinterval(.width = c(.66, .95)) +
  
  scale_fill_manual(values = c("#E91E63", "#F8BBD0")) +
  
  labs(x = "Predicted Autism Score (Latent Log-Odds)", 
       y = "",
       title = "PGS * Rater Type") +
       
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(face = "bold"))


#  --- PGS * sex --- 

slopes_pgs_sex <- slopes(
  fit_m4_bayesian,
  variables = "PGS_scaled",        
  by = "sex",        
  type = "link"
)

# 2. Extract the posterior draws
draws_slopes_pgs_sex <- posterior_draws(slopes_pgs_sex)

# 3. Create the plot
draws_slopes_pgs_sex %>%
  # mutate(group = paste(rater_type, sex, sep = " x ")) %>%
  ggplot(aes(x = draw, y = sex)) +
  
  # The ROPE gray box (Null region) 
  # Note: For means, the "0" line is just a relative point, 
  # but keeping the gray box helps maintain the aesthetic of the original paper.
  annotate("rect", xmin = -0.02, xmax = 0.02, ymin = -Inf, ymax = Inf, 
           fill = "gray", alpha = 0.4) +
  
  geom_vline(xintercept = 0, color = "darkgray", linetype = "solid") +
  
  # The "Pink Slab" aesthetic
  stat_slab(aes(fill = after_stat(cut_cdf_qi(cdf, .width = c(.95, 1)))), 
            alpha = 0.6, show.legend = FALSE) +
  stat_pointinterval(.width = c(.66, .95)) +
  
  scale_fill_manual(values = c("#E91E63", "#F8BBD0")) +
  
  labs(x = "Predicted Autism Score (Latent Log-Odds)", 
       y = "",
       title = "PGS * Sex") +
       
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(face = "bold"))

# save figure
ggsave("results/figures/bayesian/test_pgs_sex.png", width = 8, height = 6)


# --- PGS * rater_type * sex ---

slopes_pgs_rater_sex <- slopes(
  fit_m4_bayesian,
  variables = "PGS_scaled",        
  by = c("rater_type", "sex"),        
  type = "link"
)
p_direction(slopes_pgs_rater_sex)

# 2. Extract the posterior draws
draws_slopes_pgs_rater_sex <- posterior_draws(slopes_pgs_rater_sex)

# 2. Plotting code
draws_slopes_pgs_rater_sex %>%
  mutate(OR = exp(draw)) %>% # Convert Log-Odds to Odds Ratio
  ggplot(aes(x = OR, y = rater_type, fill = sex, color = sex)) +
  
  # The ROPE (Negligible effect: 0.98 to 1.02)
  annotate("rect", xmin = 0.98, xmax = 1.02, ymin = -Inf, ymax = Inf, 
           fill = "gray", alpha = 0.4) +
  
  # Reference line at 1 (No genetic effect)
  geom_vline(xintercept = 1, color = "black", size = 0.8) +
  
  # The Slabs (Density)
  stat_slab(aes(alpha = after_stat(cut_cdf_qi(cdf, .width = c(.95, 1)))), 
            # position = position_dodge(width = 0.6),
            show.legend = TRUE) +
  
  # Point interval
  stat_pointinterval(.width = c(.66, .95), 
                     position = position_dodge(width = 0.05)) +
  
  # Colors
  scale_fill_manual(values = c("FEMALE" = "#FFBB09", "MALE" = "#00C07B")) +
  scale_color_manual(values = c("FEMALE" = "#FFBB09", "MALE" = "#00C07B")) +
  scale_alpha_manual(values = c(0.5, 0.3), guide = "none") +
  
  # X-axis for Odds Ratio (Focus on 0.9 to 1.4 based on your results)
  scale_x_continuous(limits = c(0.9, 1.4), breaks = seq(0.9, 1.4, 0.1)) +
  
  labs(
    x = "Effect of PGS (Odds Ratio per SD)",
    y = "Rater Type",
    title = "PGS Effect on Autism Traits by Rater and Sex",
    subtitle = "Odds Ratios > 1 indicate higher PGS predicts higher symptoms"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# save figure
ggsave("results/figures/bayesian/test_pgs_rater_sex.png", width = 8, height = 6)
