# This script contains the column names of the variables used in the analyses

# wanted columns genotype data
geno_cols_general = c("FISNumber",
                      "EUR_1KG_Outlier", # outliers from 1KG EUR PCA
                    #   "P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1", # PGS determined to have the highest R2 in testing in 05_pgs_testing.R OLD SUMSCORES
                      "SBayesRC_SCORE_AutismSpectrumDisorder_MRG18_SBRC", # 
                      "PLATFORM",
                      "PC1_1KG", "PC2_1KG", "PC3_1KG", "PC4_1KG", "PC5_1KG", "PC6_1KG", "PC7_1KG", "PC8_1KG", "PC9_1KG", "PC10_1KG")

# general columns phenotype data
pheno_cols_general = c("FISNumber", "sex", "FamilyNumber", "twzyg")

# group columns by rater in phenotype data for the So et al. items
items_m12 = c( "q1m12", "q9m12", "q17m12", "q42m12", "q62m12", "q66m12", "q79m12", "q80m12", "q84m12", "q111m12")
items_v12 = c("q1v12", "q9v12", "q17v12", "q42v12", "q62v12", "q66v12", "q79v12", "q80v12", "q84v12", "q111v12")
items_t12 = c("q1t12", "q9t12", "q17t12", "q42t12", "q62t12", "q66t12", "q79t12", "q80t12", "q84t12", "q111t12")
items_ysr14 = c("q1ysr14", "q9ysr14", "q17ysr14", "q42ysr14", "q62ysr14", "q66ysr14", "q79ysr14", "q84ysr14", "q111ysr14") # note: item 80 is not here as it differs with the other questionnaires

items_ysr12 = c("q1_ysr12", "q9_ysr12", "q17_ysr12", "q42_ysr12", "q62_ysr12", "q66_ysr12", "q79_ysr12", "q84_ysr12", "q111_ysr12") # this is only for the sensitivity analysis of a small subpopulation of ysr at age 12
items_bs2 = c("q1y_bs2", "q9y_bs2", "q17y_bs2", "q42y_bs2", "q62y_bs2", "q66y_bs2", "q79y_bs2", "q84y_bs2", "q111y_bs2")    
items_ae2 = c("q1y_ae2", "q9y_ae2", "q17y_ae2", "q42y_ae2", "q62y_ae2", "q66y_ae2", "q79y_ae2", "q84y_ae2", "q111y_ae2")
items_c12 = c("q1y_c12", "q9y_c12", "q17y_c12", "q42y_c12", "q62y_c12", "q66y_c12", "q79y_c12", "q84y_c12", "q111y_c12")


# for the sensitivity analysis two items were excluded (q1 and q80). 
# Q1 was biased for the self scale in the measurement invariance investigations. Q80 is not available in the YSR questionnaire
items_m12_sensitivity = c("q9m12", "q17m12", "q42m12", "q62m12", "q66m12", "q79m12","q84m12", "q111m12")
items_v12_sensitivity = c("q9v12", "q17v12", "q42v12", "q62v12", "q66v12", "q79v12", "q84v12", "q111v12")
items_t12_sensitivity = c("q9t12", "q17t12", "q42t12", "q62t12", "q66t12", "q79t12", "q84t12", "q111t12")
items_ysr14_sensitivity = c("q9ysr14", "q17ysr14", "q42ysr14", "q62ysr14", "q66ysr14", "q79ysr14", "q84ysr14", "q111ysr14") # note: item 80 is not here as it differs with the other questionnaires
items_ysr12_sensitivity = c("q9_ysr12", "q17_ysr12", "q42_ysr12", "q62_ysr12", "q66_ysr12", "q79_ysr12", "q84_ysr12", "q111_ysr12") # this is only for the sensitivity analysis of a small subpopulation of ysr at age 12

# for the sensitivity analysis in the projects that contain ysr at age 12
items_bs2 = c("q1y_bs2", "q9y_bs2", "q17y_bs2", "q42y_bs2", "q62y_bs2", "q66y_bs2", "q79y_bs2", "q84y_bs2", "q111y_bs2")    
items_ae2 = c("q1y_ae2", "q9y_ae2", "q17y_ae2", "q42y_ae2", "q62y_ae2", "q66y_ae2", "q79y_ae2", "q84y_ae2", "q111y_ae2")
items_c12 = c("q1y_c12", "q9y_c12", "q17y_c12", "q42y_c12", "q62y_c12", "q66y_c12", "q79y_c12", "q84y_c12", "q111y_c12")

# wanted columns phenotype data
pheno_cols_mother = c("agem12", "in_YS_12M", "invjrm12")
pheno_cols_father = c("agev12", "in_YS_12V", "invjrv12")
pheno_cols_teacher = c("agetrf12", "in_YS_TRF12", "genderlkrt12", "invjrt12")
pheno_cols_ysr = c("ages14", "in_YS_DHBQ14", "invjrs14")
pheno_cols_ysr12 = c("age_ysr_c12", "age_ysr_ae2", "age_ysr_bs2", "in_YE_COG12", "in_YE_ATTEF2", "in_YC_BS2", "invjr_ysr_c12", "invjr_ysr_ae2", "invjr_ysr_bs2") # this is for the sensitivity analysis of ysr at age 12 where we will create a new variable with the sum of the items of the ysr at age 12 and the items of the bs2, ae2 and c12 which are available in the projects that contain ysr at age 12

# combine the wanted columns from the phenotype data
pheno_cols_mother = c(items_m12, pheno_cols_mother)
pheno_cols_father = c(items_v12, pheno_cols_father)
pheno_cols_teacher = c(items_t12, pheno_cols_teacher)
pheno_cols_ysr = c(items_ysr14, pheno_cols_ysr) # add the items ae2, bs2 ,c12 for the sensitivity analysis of ysr at age 12 here as well
pheno_cols_ysr12 = c(items_ae2, items_bs2, items_c12, pheno_cols_ysr12) # add the items ae2, bs2 ,c12 for the sensitivity analysis of ysr at age 12 here as well