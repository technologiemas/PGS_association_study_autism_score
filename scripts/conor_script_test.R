rm(list = ls(all = TRUE))
gc()

library(ordinal)
library(MASS)
#
N=10000
#
R2=.1 # effect size AGE predicts liability (continuous) Y with R2 of .10.
#     # 
RE = matrix(c(1,.5,.5,1),2,2)  # the clustering correlated residuls
#
icc = RE[2,1] # intraclass correlation is var(b0) / (var(b0) + var(E*))
#             # here var(b0) = .5 and var(E*) = 1-.5 (so that var(E), residual variance = 1, var(E))
#             # so .5 / (.5+.5) = 1
#             # the residual variance is var(E) = 1 = var(E*) + var(b0), this variance of 1 is partitioned in to var(b0) and var(E*)
#  
#
b0=0
b1=sqrt(R2/(1-R2))  # residual variance = 1
sde = 1             # ................... = 1
SDE = diag(c(sde, sde))
SE = SDE%*%RE%*%t(SDE)
AGE=rnorm(N) # 
vAGE = var(AGE)
e=mvrnorm(N, rep(0,2), Sigma = SE)
#
Y1=b0 + b1*AGE + e[,1]
Y2=b0 + b1*AGE + e[,2]
idfam=1:N
datW = matrix(0, N, 1+2+1)
datW[,1]=idfam
datW[,2]=AGE
datW[,3]=Y1
datW[,4]=Y2
# into long
datL = as.data.frame( matrix(0,N*2, 4) )
ii = 0
for (i in 1:N) {
for (j in 1:2) {
ii =ii+1
datL[ii,1]=idfam[i]
datL[ii,2]=AGE[i]
if (j == 1) datL[ii,3]=Y1[i]
if (j == 2) datL[ii,3]=Y2[i]
}}
colnames(datL) = c("famid","AGE","YC","Y3")
#
# ordinary  
prob012 = c(.25,.45,.3)  # sum(prob0123) = 1
vph=b1**2 + sde**2

thr1 = qnorm(prob012[3], 0, sqrt(vph))
thr2 = qnorm(sum(prob012[3:2]), 0, sqrt(vph))
#
datL[datL[,3]>thr1,4]=1
datL[datL[,3]>thr2,4]=2
round(cor(datL[,3:4]),4)  # should be large, say >.8, same variable YC continuous, Y3 ordinal
# 
#
datL[,4] = as.factor(datL[,4])   # This is the 0,1,2 version of the continuous Y
#
# out1 is the model with random effects
#
out1 = clmm(Y3~1 + AGE + (1 | famid), link = "probit", threshold='flexible', data=datL)
#
# out2 ignores the random intercept (1|famid)
#
out2 = clm(Y3~1+AGE , link = "probit", threshold='flexible', data=datL)
#
# 
sout2 = summary(out2)$coefficients
sout1 = summary(out1)$coefficients
#
#
# results in out2 (no random intercept 
sout2  # the thresholds and the parameter b1
# the true values are
est2 = c(thr1, thr2, b1)
sout2_ = cbind(est2, sout2)
colnames(sout2_)[1] = 'true'
round(sout2_,5) # results
#
# R2 
#
vE=1 # scaling in this parameterization residual var == 1
b2est_=sout2[3,1]
R2est = (vAGE*b2est_**2)  / (vAGE*b2est_**2 + vE)
print(round(c(R2, R2est),5) )
#
# here are the estimated thresholds and the true values
round(out2$Theta,4)
c(thr1, thr2)
# the results in the random intercept model are more complicated
#
#
sdB0 = as.numeric( out1$ST$famid ) # stdev of b0
vB0 = sdB0**2
vE = 1  # still scaling
# the scaling of the residual is var(E) = 1, but now the var(E) is the residual, var(E*)
#                                            taking into account the random effects (var(B0))
#                                             the var(E) in out2 is ==1, but now var(E*) is 1.0 
#
b2est_1 = sout1[3,1]
icc_ = vB0 /(vB0 + vE)  # this should equal the icc which follows from the definition of RE and icc above
print( round(c(icc_, icc),3) )
#
# so we have var(E) = vB0 + vE, where now vE = 1 is the var(E*)
# b2est_1 is the regression coefficient. 
# this will not equal b2est_ (from out2) because in out var(E) = 1, but in out2 var(E) = var(b0+var(E*)) = var(b0 + 1)
#
# 
R2_est_1 = (vAGE*b2est_1**2) / (vAGE*b2est_1**2 + vB0 + vE)
print(round(c(R2, R2_est_1),5) )
#
# the thresholds in out1 do not resemble thr1 and thr2, again because of the scaling
# here are the true thr1 and thr2 based on the data simulation
print(c(thr1, thr2))
# here are the thresholds from the out2 output (no random b0)
print(out2$Theta)
# but here are the thrsholds from the out1 output
print(out1$Theta)
#
thr1_1 = out1$Theta[1]
thr1_2 = out1$Theta[2]
# but we know that these are expressed on a scale with variance equal to 
# vAGE*b2est_1**2 + vB0 + vE, so to see the orginal values
print(round(c(thr1_1 /sqrt(vAGE*b2est_1**2 + vB0 + vE),thr1_2 /sqrt(vAGE*b2est_1**2 + vB0 + vE)),4)   ) 
 c(thr1, thr2)

# vB0 is the variance of the random intercept
# sdB0 = as.numeric( out1$ST$famid ) # stdev of b0
# vB0 = sdB0**2
# vE = 1, is this always the case?

# in conclusion the r2 of age is:
# R2_est_1 = (vAGE*b2est_1**2) / (vAGE*b2est_1**2 + vB0 + vE)




cov2cor(vcov(out1))
vcov(out1)
