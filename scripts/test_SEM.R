library(lavaan)
# library(semPlot)
library(lavaanPlot)
library(dplyr)

# load data
data = readRDS("data/processed/02_full_dataset_clean.rds")

data = rename(data, "PGS" = "P_0_1_SCORE_AutismSpectrumDisorder_MRG18_LDp1")

model <- '
# latent variable autism_score
autism_score =~ lt*t12_aut_sum + lm*m12_aut_sum + lv*v12_aut_sum + ls*ysr14_aut_sum

# regressions
autism_score ~ b*PGS
t12_aut_sum ~ bt*PGS
m12_aut_sum ~ bm*PGS
v12_aut_sum ~ bv*PGS
ysr14_aut_sum ~ bs*PGS

# residual variances of observed indicators
t12_aut_sum ~~ res_t*t12_aut_sum
m12_aut_sum ~~ res_m*m12_aut_sum
v12_aut_sum ~~ res_v*v12_aut_sum
ysr14_aut_sum ~~ res_s*ysr14_aut_sum

# residual variance of latent variable
PGS ~~ 1*PGS
res_a ~~ 1*res_a

# correlated residual between autism_score and res_a
autism_score ~~ se*res_a
'

fit <- sem(model, data = data)
pl <- lavaanPlot(model = fit)
pl

# Example for saving to .png
save_png(pl, "plot.png")
