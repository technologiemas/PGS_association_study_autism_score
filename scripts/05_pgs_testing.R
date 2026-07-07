# --- SCRIPT 05: TESTING PGS DATA ---
# which thresholded PGS is best associated with autism score so that we can use it in further analysis?

rm(list = ls(all = TRUE))
gc()

library(dplyr)
library(haven)


# --- LOAD DATA ---
data = readRDS("data/processed/02_full_dataset_long.rds")
data_mother = data %>%
  filter(rater == "Mother") %>%
  select(-rater)

genotype_data = read_sav("./data/raw/NTR-DSR-5023_AutismSpectrumDisorder_PMID30804558_MRG18_PedMergedWithScores.sav")

all_pgs = genotype_data %>%
  select("FISnumber",
"P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1",
"P_0_05_SCORE_AutismSpectrumDisorder_MRG18_LDp1",
"P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1" ,
"P_0_2_SCORE_AutismSpectrumDisorder_MRG18_LDp1" ,
"P_0_3_SCORE_AutismSpectrumDisorder_MRG18_LDp1" ,
"P_0_5_SCORE_AutismSpectrumDisorder_MRG18_LDp1" ,
"P_inf_SCORE_AutismSpectrumDisorder_MRG18_LDp1" )


# change colname FISNumber to FISnumber for consistency with phenotype data
all_pgs = all_pgs %>% rename(FISNumber = FISnumber)

# see correlation of all pgs in df
correlations_pgs = cor(all_pgs)

#scale all pgs
all_pgs = all_pgs %>%
  mutate(across(everything(), ~ as.numeric(scale(.)), .names = "{col}_scaled"))

# rename PCs
data_mother = data_mother %>%
  rename(PC1 = PC1_1KG,
         PC2 = PC2_1KG,
         PC3 = PC3_1KG,
         PC4 = PC4_1KG,
         PC5 = PC5_1KG,
         PC6 = PC6_1KG,
         PC7 = PC7_1KG,
         PC8 = PC8_1KG,
         PC9 = PC9_1KG,
         PC10 = PC10_1KG)

# join all_pgs and mother on FISNumber
all_pgs = all_pgs %>%
  mutate(FISNumber = as.factor(FISNumber))
data_mother = data_mother %>%
  left_join(all_pgs, by = "FISNumber")

# --- SCALING/STANDARDIZING THE DATA---

# all continuous variables will be scaled to mean = 0 and sd = 1 (e.g. age, pgs, pcs)
# autism score will be ordinalized with three levels: 0, 1-3, 4+ (no, mild, high)
# all categorical variables will be converted to factors (e.g., PLATFORM, rater, sex)

data_mother = data_mother %>%
  mutate(across(c("agem12", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "m12_aut_sum"),
                ~ as.numeric(scale(.)), .names = "{col}_scaled"))

data_mother = data_mother %>%
  mutate(PLATFORM = haven::zap_labels(PLATFORM), sex = haven::zap_labels(sex)) %>%
  mutate(across(c(PLATFORM, sex, FISNumber, FamilyNumber), as.factor))

# --- MODELING ---
library(ordinal)

# function to run ordinal logistic regression for each pgs with covariates and random effect for family
run_ordinal_logistic_regression = function(data, pgs_col) {
  formula = as.formula(paste("autism_score_ordinal ~", pgs_col, "+ (1 | FamilyNumber) + age_scaled + sex + PLATFORM + PC1_scaled + PC2_scaled + PC3_scaled + PC4_scaled + PC5_scaled + PC6_scaled + PC7_scaled + PC8_scaled + PC9_scaled + PC10_scaled"))
  model = clmm(formula, data = data, Hess = TRUE)
  return(summary(model))
}

# list of pgs columns to test
pgs_columns = c("P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1_scaled",
                "P_0_05_SCORE_AutismSpectrumDisorder_MRG18_LDp1_scaled",
                "P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1_scaled",
                "P_0_2_SCORE_AutismSpectrumDisorder_MRG18_LDp1_scaled",
                "P_0_3_SCORE_AutismSpectrumDisorder_MRG18_LDp1_scaled",
                "P_0_5_SCORE_AutismSpectrumDisorder_MRG18_LDp1_scaled",
                "P_inf_SCORE_AutismSpectrumDisorder_MRG18_LDp1_scaled")

# run models and store results
model_results = lapply(pgs_columns, function(col) {
  result = run_ordinal_logistic_regression(data_mother, col)
  return(list(pgs = col, result = result))
})

# extract pgs coefficients and p-values
pgs_coefficients = data.frame()
for (res in model_results) {
  pgs_name = res$pgs
  coef_info = res$result$coefficients[pgs_name, ]
  pgs_coefficients = rbind(pgs_coefficients, data.frame(
    PGS = pgs_name,
    Estimate = coef_info["Estimate"],
    Std_Error = coef_info["Std. Error"],
    z_value = coef_info["z value"],
    p_value = coef_info["Pr(>|z|)"]
  ))
}

model_results
pgs_coefficients
correlations_pgs

# save results
dir.create("results/pgs_threshold_comparisons", showWarnings = FALSE)

saveRDS(model_results, "results/pgs_threshold_comparisons/models.rds")
saveRDS(pgs_coefficients, "results/pgs_threshold_comparisons/coefficients.rds")
saveRDS(correlations_pgs, "results/pgs_threshold_comparisons/correlations_pgs.rds")

# model_results = readRDS("results/pgs_threshold_comparisons/models.rds")
# pgs_coefficients = readRDS("results/pgs_threshold_comparisons/coefficients.rds")
# correlations_pgs = readRDS("results/pgs_threshold_comparisons/correlations_pgs.rds")

# conclusion: they are very similar, we pick p_0_1 
