# This script does power calculations for the main effect of PGS on autism score ordinal
# It calculates the effective sample size accounting for clustering within families and participants by using
# adapted from script at our department

rm(list = ls(all = TRUE))
gc()

library(lme4)
library(lmerTest)
library(dplyr)
library(purrr)
library(tidyr)
library(pwr)

data_long <- readRDS("data/processed/02_full_dataset_long.rds")

subgroups <- data_long %>% 
  distinct(rater, sex)

per_subgroup_results <- map_df(1:nrow(subgroups), function(i) {
  current_rater <- subgroups$rater[i]
  current_sex   <- subgroups$sex[i]
  
  sub_data <- data_long %>% 
    filter(rater == current_rater, sex == current_sex)
  
    mod <- lmer(autism_score ~ 1 + (1 | FamilyNumber), data = sub_data)
    m_data <- model.frame(mod)
    
    # family variance components
    vc <- as.data.frame(VarCorr(mod))
    tot_v <- sum(vc$vcov)
    icc_f <- vc$vcov[grepl("FamilyNumber", vc$grp)] / tot_v
    
    eff_res <- m_data %>%
      group_by(FamilyNumber) %>% 
      mutate(n_family = n()) %>%
      ungroup() %>%
      mutate(
        DE_row = 1 + (n_family - 1) * icc_f,
        weight = 1 / DE_row
      ) %>%
      summarise(
        Rater       = current_rater,
        Sex         = current_sex,
        Total_Raw_N = n(),
        Effective_N = as.integer(sum(weight)),
        Subgroup_ICC = icc_f
      )
    
    return(eff_res)
})

# power calculation for the main effect of PGS using an effect size from our previous systematic review 
effect_size = 0.02
per_subgroup_results = per_subgroup_results %>%
  mutate(
    power_pgs = pwr.f2.test(u = 1, v = Effective_N, f2 = effect_size, sig.level= 0.05)$power
  )

per_subgroup_results

pwr.f2.test(u = 1, f2 = effect_size, sig.level= 0.05, power=0.8) # this is for the systematic review
