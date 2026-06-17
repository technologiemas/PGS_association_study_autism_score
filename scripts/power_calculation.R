# This script does power calculations for the main effect of PGS on autism score ordinal
# It calculates the effective sample size accounting for clustering within families and participants

rm(list = ls(all = TRUE))
gc()

library(pwr)

# sample sizes
tot_female = 8831
tot_male = 6624
m_female = 3002
m_male = 2244
v_female = 2315
v_male = 1683
t_female = 1910
t_male = 1551
s_female = 1963
s_male = 1205

# effect size
effect_size = 0.02 # average beta from systematic review

# calculating ICC
data = readRDS("data/processed/02_full_dataset_clean.rds")
data_long = readRDS("data/processed/02_full_dataset_long.rds")
# calculate average FamilyNumber size
m_family = mean(table(data$FamilyNumber))
m_participant = mean(table(data$FISNumber))

# calculate ICC for autism_score by FamilyNumber
library(lme4)
library(lmerTest)
model_icc = lmer(autism_score ~ 1 + (1 | FamilyNumber) + (1 | FISNumber), data = data_long)

v <- as.data.frame(VarCorr(model_icc))$vcov
icc_family <- v[1] / sum(v)
icc_participant <- v[2] / sum(v)

icc_overall = (v[1] + v[2]) / sum(v)


# effect_sample_size
# eff_n = function(n, m_family, m_participant, icc_family, icc_participant) {
#   n / ((1 + (m_family - 1) * icc_family) * (1 + (m_participant - 1) * icc_participant))
# }

# eff_n = function (n, m_family, m_participant, icc_overall) {
#   n / (1 + (m_family * m_participant - 1) * icc_overall)
#   n / DE
# }

eff_n = function(n, m_family, m_participant, icc_family, icc_participant) {
  DE =  1 + (m_family - 1) * icc_family + (m_participant - 1) * icc_participant
  n / DE
}

tot_female = eff_n(tot_female, m_family, m_participant, icc_family, icc_participant)
tot_male = eff_n(tot_male, m_family, m_participant, icc_family, icc_participant)
m_female = eff_n(m_female, m_family, m_participant, icc_family, icc_participant)
m_male = eff_n(m_male, m_family, m_participant, icc_family, icc_participant)
v_female = eff_n(v_female, m_family, m_participant, icc_family, icc_participant)
v_male = eff_n(v_male, m_family, m_participant, icc_family, icc_participant)
t_female = eff_n(t_female, m_family, m_participant, icc_family, icc_participant)
t_male = eff_n(t_male, m_family, m_participant, icc_family, icc_participant)
s_female = eff_n(s_female, m_family, m_participant, icc_family, icc_participant)
s_male = eff_n(s_male, m_family, m_participant, icc_family, icc_participant)

# calculating power
pow_tot_female = pwr.f2.test(u = 1, v = tot_female, f2 = effect_size, sig.level= 0.05)$power
pow_tot_male = pwr.f2.test(u = 1, v = tot_male, f2 = effect_size, sig.level= 0.05)$power
pow_m_female = pwr.f2.test(u = 1, v = m_female, f2 = effect_size, sig.level= 0.05)$power
pow_m_male = pwr.f2.test(u = 1, v = m_male, f2 = effect_size, sig.level= 0.05)$power
pow_v_female = pwr.f2.test(u = 1, v = v_female, f2 = effect_size, sig.level= 0.05)$power
pow_v_male = pwr.f2.test(u = 1, v = v_male, f2 = effect_size, sig.level= 0.05)$power
pow_t_female = pwr.f2.test(u = 1, v = t_female, f2 = effect_size, sig.level= 0.05)$power
pow_t_male = pwr.f2.test(u = 1, v = t_male, f2 = effect_size, sig.level= 0.05)$power
pow_s_female = pwr.f2.test(u = 1, v = s_female, f2 = effect_size, sig.level= 0.05)$power
pow_s_male = pwr.f2.test(u = 1, v = s_male, f2 = effect_size, sig.level= 0.05)$power

cat("power for total female:", pow_tot_female, "\n")
cat("power for total male:", pow_tot_male, "\n")
cat("power for mother rated males:", pow_m_male, "\n")
cat("power for mother rated females:", pow_m_female, "\n")
cat("power for father rated males:", pow_v_male, "\n")
cat("power for father rated females:", pow_v_female, "\n")
cat("power for teacher rated males:", pow_t_male, "\n")
cat("power for teacher rated females:", pow_t_female, "\n")
cat("power for self rated males:", pow_s_male, "\n")
cat("power for self rated females:", pow_s_female, "\n")

# create dataframe with results
power_results = data.frame(
  group = c("total_female", "total_male", "mother_female", "mother_male", "father_female", "father_male", "teacher_female", "teacher_male", "self_female", "self_male"),
  n = c(tot_female, tot_male, m_female, m_male, v_female, v_male, t_female, t_male, s_female, s_male),
  power = c(pow_tot_female, pow_tot_male, pow_m_female, pow_m_male, pow_v_female, pow_v_male, pow_t_female, pow_t_male, pow_s_female, pow_s_male)
)     


