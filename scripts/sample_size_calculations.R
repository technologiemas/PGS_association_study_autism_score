MALE = 1
FEMALE = 2

# Load the data
data = readRDS("./data/processed/full_required_dataset.rds")

# calculate total sample size 
total_n = length(data$FISnumber)

# calculate sample size for both sexes separately
female_n = length(data$FISnumber[data$sex == FEMALE])
male_n = length(data$FISnumber[data$sex == MALE])

# calculate sample size for both sexes separately, for each phenotype group separately
female_n_m12 = length(data$FISnumber[data$sex == FEMALE & !is.na(data$q1m12)])
male_n_m12 = length(data$FISnumber[data$sex == MALE & !is.na(data$q1m12)])
female_n_v12 = length(data$FISnumber[data$sex == FEMALE & !is.na(data$q1v12)])
male_n_v12 = length(data$FISnumber[data$sex == MALE & !is.na(data$q1v12)])
female_n_t12 = length(data$FISnumber[data$sex == FEMALE & !is.na(data$q1t12)])
male_n_t12 = length(data$FISnumber[data$sex == MALE & !is.na(data$q1t12)])
female_n_yrs14 = length(data$FISnumber[data$sex == FEMALE & !is.na(data$q1ysr14)])
male_n_yrs14 = length(data$FISnumber[data$sex == MALE & !is.na(data$q1ysr14)])

