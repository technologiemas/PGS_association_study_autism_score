library(haven)
library(dplyr)

MALE = 1
FEMALE = 2

phenotype_file_path = "./data/raw/PHE_20250325_5023_YJS.sav"
genotype_file_path = "./data/raw/NTR-DSR-5023_AutismSpectrumDisorder_PMID30804558_MRG18_PedMergedWithScores.sav"

phenotype_data = read_sav(phenotype_file_path)
genotype_data = read_sav(genotype_file_path)

# wanted columns genotype data
geno_cols_general = c("FISnumber", 
"P_0_01_SCORE_AutismSpectrumDisorder_MRG18_LDp1", 
"P_0_05_SCORE_AutismSpectrumDisorder_MRG18_LDp1", 
"P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1" , 
"P_0_2_SCORE_AutismSpectrumDisorder_MRG18_LDp1" , 
"P_0_3_SCORE_AutismSpectrumDisorder_MRG18_LDp1" , 
"P_0_5_SCORE_AutismSpectrumDisorder_MRG18_LDp1" , 
"P_inf_SCORE_AutismSpectrumDisorder_MRG18_LDp1")

# general columns phenotype data
pheno_cols_general = c("FISNumber", "sex")

# group columns by rater in phenotype data
m7 = c("q1m7", "q9m7", "q17m7", "q42m7", "q62m7", "q66m7", "q79m7", "q80m7", "q84m7", "q111m7", "agem7")
m10 = c( "q1m10", "q9m10", "q17m10", "q42m10", "q62m10", "q66m10", "q79m10", "q80m10", "q84m10", "q111m10", "agem10")
m12 = c( "q1m12", "q9m12", "q17m12", "q42m12", "q62m12", "q66m12", "q79m12", "q80m12", "q84m12", "q111m12", "agem12")
v7 = c("q1v7", "q9v7", "q17v7", "q42v7", "q62v7", "q66v7", "q79v7", "q80v7", "q84v7", "q111v7", "agev7")
v10 = c("q1v10", "q9v10", "q17v10", "q42v10", "q62v10", "q66v10", "q79v10", "q80v10", "q84v10", "q111v10", "agev10")
v12 = c("q1v12", "q9v12", "q17v12", "q42v12", "q62v12", "q66v12", "q79v12", "q80v12", "q84v12", "q111v12", "agem12")
t5 = c("q1t5", "q9t5", "q17t5", "q42t5", "q62t5", "q66t5", "q79t5", "q80t5", "q84t5", "q111t5", "agetrf5")
t7 = c("q1t7", "q9t7", "q17t7", "q42t7", "q62t7", "q66t7", "q79t7", "q80t7", "q84t7", "q111t7", "agetrf7")
t10 = c("q1t10", "q9t10", "q17t10", "q42t10", "q62t10", "q66t10", "q79t10", "q80t10", "q84t10", "q111t10", "agetrf10")
t12 = c("q1t12", "q9t12", "q17t12", "q42t12", "q62t12", "q66t12", "q79t12", "q80t12", "q84t12", "q111t12", "agetrf12")
ysr14 = c("q1ysr14", "q9ysr14", "q17ysr14", "q42ysr14", "q62ysr14", "q66ysr14", "q79ysr14", "q80ysr14", "q84ysr14", "q111ysr14", "ages14")
ysr16 = c("q1ysr16", "q9ysr16", "q17ysr16", "q42ysr16", "q62ysr16", "q66ysr16", "q79ysr16", "q80ysr16", "q84ysr16", "q111ysr16", "ages16")
ysr18 = c("q1ysr18", "q9ysr18", "q17ysr18", "q42ysr18", "q62ysr18", "q66ysr18", "q79ysr18", "q80ysr18", "q84ysr18", "q111ysr18", "ages18")

# select the columns of interest from the phenotype data
data = phenotype_data %>% select(all_of(pheno_cols_general), all_of(m12), all_of(v12), all_of(t12), all_of(ysr14))
data = data %>% rename(FISnumber = FISNumber) # change colname FISNumber to FISnumber for consistency with genotype data

# select the columns of interest from the genotype data
genotype_data = genotype_data %>% select(all_of(geno_cols_general))

# join the two data sets where data is present for both tables for each FISnumber 
data = data %>% inner_join(genotype_data, by = "FISnumber")

# save data as new dataset rds file
saveRDS(data, file = "./data/processed/full_required_dataset.rds")
