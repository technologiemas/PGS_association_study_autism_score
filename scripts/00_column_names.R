# wanted columns genotype data
geno_cols_general = c("FISNumber", 
                      "EUR_1KG_Outlier",
                      "P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1",
                      "PLATFORM", 
                      "PC1_1KG", "PC2_1KG", "PC3_1KG", "PC4_1KG", "PC5_1KG", "PC6_1KG", "PC7_1KG", "PC8_1KG", "PC9_1KG", "PC10_1KG")

# general columns phenotype data
pheno_cols_general = c("FISNumber", "sex", "FamilyNumber")

# group columns by rater in phenotype data for the So et al. items
items_m12 = c( "q1m12", "q9m12", "q17m12", "q42m12", "q62m12", "q66m12", "q79m12", "q80m12", "q84m12", "q111m12")
items_v12 = c("q1v12", "q9v12", "q17v12", "q42v12", "q62v12", "q66v12", "q79v12", "q80v12", "q84v12", "q111v12")
items_t12 = c("q1t12", "q9t12", "q17t12", "q42t12", "q62t12", "q66t12", "q79t12", "q80t12", "q84t12", "q111t12")
items_ysr14 = c("q1ysr14", "q9ysr14", "q17ysr14", "q42ysr14", "q62ysr14", "q66ysr14", "q79ysr14", "q84ysr14", "q111ysr14")

# wanted columns phenotype data
pheno_cols_mother = c("agem12", "in_YS_12M")
pheno_cols_father = c("agev12", "in_YS_12V")
pheno_cols_teacher = c("agetrf12", "in_YS_TRF12", "genderlkrt12")
pheno_cols_ysr = c("ages14", "in_YS_DHBQ14")

# combine the wanted columns from the phenotype data
pheno_cols_mother = c(items_m12, pheno_cols_mother)
pheno_cols_father = c(items_v12, pheno_cols_father)
pheno_cols_teacher = c(items_t12, pheno_cols_teacher)
pheno_cols_ysr = c(items_ysr14, pheno_cols_ysr)