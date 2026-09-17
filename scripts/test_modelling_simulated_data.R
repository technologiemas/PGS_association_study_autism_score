# This script shows on simulated data why the autism score is modelled as ordinal rather
# than as continuous: with a skewed, zero-inflated outcome the residuals of the linear
# mixed model are clearly not normally distributed.
#
# The simulation follows Conor's exact-data simulation
#   1. simulate latent A, C and E scores for parents and siblings with mvrnorm(emp = TRUE),
#      so the sample (co)variances equal the population values exactly
#   2. split A into the part captured by the PGS and the part that is not
#   3. build the phenotype as par_a * A + par_c * C + par_e * E, which makes the data
#      clustered in families (siblings share A and C).
#   4. discretize the (normal) phenotype at quantiles chosen so that the response
#      proportions are skewed, as in Conor's presp0 / cp / qs block. This is what turns
#      the normal phenotype into the zero-inflated, right-skewed shape of the ASEBA
#      autism sum score.
#
# Because the phenotype is simulated as normal and only the observed score is skewed, we
# can fit the same model to both: the skew is then the only difference between them, so it
# has to be what breaks the normality assumption.


rm(list=ls(all=TRUE))
library(MASS)
library(umx)
library(geepack)  # GEE regr
library(ggplot2)
#
# ------------------------------------------------------------------------- settings
#
cmethod='exchangeable'                 # gee error cov structure
#
#
nmz=1500  # sample size MZ pairs
ndz=1500  # sample size DZ pairs
#
#
#                              
par_as=p_as=sqrt(.8)  # A  # these values are the variance components of A, C, and E
par_es=p_es=sqrt(.05)  # E  # they add up to one - so that suggests that the phenotypic variance is 1.
par_cs=p_cs=sqrt(.15)  # C  # however, we have to take in to account cov(AC), whcih we introduce below. 
#
par_a = par_as
par_c = par_cs
par_e = par_es
#
#
ng=100  # number of diallelic loci    
ngp=2  # number of loci comprising polygenic score pgs (0 <= npg <= ng).
#
p_pgs=ngp/ng  # percentage of genetic variance explained by ngp measured loci
# 
p_A=1-p_pgs # not explained = A without pgs effect
#
#
VA1=p_A; 
VP=p_pgs;  # .... VA1+VP = VA = 1 
VC=1; 
VE=1 
#
#
# simulate data exactly: 
# the sample MZ and DZ phenotypic covariance will equal the population matrices 
# to do this we simulate data at the level of A, C, and E in mz and dz twins.
# once we have the latent variables scores on A, C, E, we use that information for create phenotypic scores.
#
#                 mother  father    twin 1      twin 2
#                 m    m   v    v   t1    t1    t2    t2 
#                 1   2    3    4   5     6     7      8   9  10   11
#
# create the covariance matrix:
SLmz=SLdz=diag(c(VA1, VP, VA1, VP, VA1/2, VP/2,VA1/2, VP/2,VC, VE, VE))
#
# simulate the latent variables exactly 
#
Ldz=mvrnorm(ndz, rep(0,11), Sigma=SLdz, emp=T)  # emp=T means exact data simulation, cov(Ldz) = SLdz
Lmz=mvrnorm(nmz, rep(0,11), Sigma=SLmz, emp=T)  # emp=T means exact data simulation, cov(Lmz) = SLmz  # 
#
# build the exact simulated data DZ  ................... DZ group
# Here are the 
#
Am_=Ldz[,1]   # additive genetic score of mother
Pm=Ldz[,2]    # additive genetic polygenic score (prs) of the mother
Af_=Ldz[,3]   # additive genetic score of father
Pf=Ldz[,4]    # prs father
At1r=Ldz[,5]  # dz twin 1 additive genetic score .... residual
Pt1r=Ldz[,6]  # dz twin 1 prs    .... residual
At2r=Ldz[,7]  # dz twin 2 additive genetic score .... residual
Pt2r=Ldz[,8]  # dz twin 2 prs .... residual
#
At1_=.5*Am_+.5*Af_+At1r # A res t1  # additive genetic score of twin1 ... note the add gen score of twin one is due to mother, father and residual.  
Pt1=.5*Pm+.5*Pf+Pt1r # Prs t1  # additive genetic prs score of twin 1
At2_=.5*Am_+.5*Af_+At2r #t2   # additive genetic score of twin 2
Pt2=.5*Pm+.5*Pf+Pt2r# t2   # additive genetic prs score of twin  
C=Ldz[,9]   # shared environmental score
E1=Ldz[,10]  # unshared environmental scores twin 1
E2=Ldz[,11]  # unshared environmental scores twin 2
#
# total additive genetic (A) scores. the A consist of the PRS part and the rest, these are combined here
# 
Am=Am_+Pm    # total mother additive genetic score
Af=Af_+Pf    # total father additive genetic score
At1=At1_+Pt1     # total twin1 additive genetic score
At2=At2_+Pt2     # total twin2 additive genetic score
#
Pht1=par_a*At1 + par_e*E1 + par_c*C  
Pht2=par_a*At2 + par_e*E2 + par_c*C  
#
stzPRS=FALSE # TRUE # standardize the prs 
if (stzPRS) {
  Pm=scale(Pm)
  Pf=scale(Pf)
  Pt1=scale(Pt1)
  Pt2=scale(Pt2)
}
#
exdatdz=as.data.frame(cbind(Pm, Pf, Pt1, Pt2, Pht1, Pht2))  # PRS scores and the phenotypic scores of the twins
#  exdatdzA=as.data.frame(cbind(Am, Af, At1, At2, Pht1, Pht2))
#
# build the exact simulated data MZ
Am_=Lmz[,1]
Pm=Lmz[,2]
Af_=Lmz[,3]
Pf=Lmz[,4]
#
At1r=Lmz[,5]
Pt1r=Lmz[,6]
At2r=Lmz[,5] # MZ MZ MZ MZ MZ this is Ldz[,7] in the DZ, but the MZ are identical
Pt2r=Lmz[,6] # MZ MZ MZ MZ MZ this is Ldz[,8]
#
#
At1_=.5*Am_+.5*Af_+At1r # A res
Pt1=.5*Pm+.5*Pf+Pt1r # Prs
At2_=At1_ #.5*Am_+.5*Af_+At2r 
Pt2 = Pt1 # MZ MZ MZ MZ MZ ...............5*Pm+.5*Pf+Pt2r # 
C=Lmz[,9]
E1=Lmz[,10]
E2=Lmz[,11]
#
Am=Am_+Pm
Af=Af_+Pf
#
At1=At1_+Pt1
At2=At1 # At2_+Pt2  
#
#Pht1_=par_a*At1 + par_e*E1 + par_c*C
#Pht2_=par_a*At2 + par_e*E2 + par_c*C
Pht1=par_a*At1 + par_e*E1 + par_c*C
Pht2=par_a*At2 + par_e*E2 + par_c*C
#
## standardize prs ?  
# DO no. 
stzPRS=FALSE # TRUE # standardize the prs
#
if (stzPRS) {
  Pm=scale(Pm)
  Pf=scale(Pf)
  Pt1=scale(Pt1)
  Pt2=scale(Pt2)
}
#
exdatmz=as.data.frame(cbind(Pm, Pf, Pt1, Pt2, Pht1, Pht2))  #
# exdatmzA=as.data.frame(cbind(Am, Af, At1, At2, Pht1, Pht2))
#
colnames(exdatdz) =colnames(exdatmz) = c('pgsm','pgsf','pgst1','pgst2','pht1','pht2')
#
# this concludes the data simulation. At this point we have MZ and DZ families with 
# phenotypic values in the twins and PRS scores in the twins and their parents. 
# 
# below we shall only use the data of the twins, discarding the parental PRS scores. 
#
# summary statistics correlaions and variances
#
round(cor(exdatmz),4)      -> phrmz_
round(cor(exdatdz),4)      -> phrdz_
round(apply(exdatmz,2,var),4)[6] -> phvarmz_
round(apply(exdatdz,2,var),4)[6] -> phvardz_
#
# --------------------------------- end exact data simualtion
# exdatdz and exdatmz are dataframes, exact data simulation
# when analysing these data, we should "get out" what we "put in"
#
# exact data in the wide (horizontal) format
#
phdatmz_e = as.data.frame(exdatmz[,3:6])  # the MZ data without the parental PRS
phdatdz_e = as.data.frame(exdatdz[,3:6]) # the MZ data without the parental PRS
colnames(phdatmz_e)=colnames(phdatdz_e) =c('pgst1','pgst2','pht1','pht2')
#
# select the variable to analyze (dropping the twin member index)
selDVs=c('pht')	# the name of the variable to analyse (pht1 pht2)
#			# in the data. Frame the variable is called weight and weight
#			# but we do not have to add the twin nr info. 
#
# use umx to fit the ACE model.
#
selCovs=c('pgst')
PHENO_M1=umxACEv(name='PHENO_AC_DE',selDVs,selCovs=selCovs,dzCr = 1,dzData=phdatdz_e, mzData=phdatmz_e, sep="") 
#
summary(PHENO_M1)
#
tmp=c(phdatmz_e$pht1,phdatmz_e$pht2,phdatdz_e$pht1,phdatdz_e$pht2)
# 
phdatdz_e_o = phdatdz_e
phdatmz_e_o = phdatmz_e
#
nresp=3
#          0,  1, 2
presp0=c( .3,.4,.3)
presp0 = presp0/sum(presp0) # should add up to 1
cp=rep(0,nresp)
for (i in 1:nresp) {
  for (j in 1:i) {
    cp[i]=cp[i]+presp0[j]
  }}
#
#head(phdatdz_e)
#head(phdatmz_e)
#head(phdatdz_e_o)
#head(phdatmz_e_o)
#                           0  1  2   3   4   
qs=quantile(tmp, probs=cp)
qs=round(qs,2)
#
phdatmz_e_o$pht1=0  # 
sel=phdatmz_e$pht1<=qs[1]
phdatmz_e_o$pht1[sel]=0
sel=phdatmz_e$pht1>qs[1]
phdatmz_e_o$pht1[sel]=1
sel=phdatmz_e$pht1>=qs[2]
phdatmz_e_o$pht1[sel]=2
#
phdatmz_e_o$pht2=0  # 
sel=phdatmz_e$pht2<=qs[1]
phdatmz_e_o$pht2[sel]=0
sel=phdatmz_e$pht2>qs[1]
phdatmz_e_o$pht2[sel]=1
sel=phdatmz_e$pht2>=qs[2]
phdatmz_e_o$pht2[sel]=2

#
phdatdz_e_o$pht1=0  # 
sel=phdatdz_e$pht1<=qs[1]
phdatdz_e_o$pht1[sel]=0
sel=phdatdz_e$pht1>qs[1]
phdatdz_e_o$pht1[sel]=1
sel=phdatdz_e$pht1>=qs[2]
phdatdz_e_o$pht1[sel]=2
#
phdatdz_e_o$pht2=0  # 
sel=phdatdz_e$pht2<=qs[1]
phdatdz_e_o$pht2[sel]=0
sel=phdatdz_e$pht2>qs[1]
phdatdz_e_o$pht2[sel]=1
sel=phdatdz_e$pht2>=qs[2]
phdatdz_e_o$pht2[sel]=2
#
table(phdatmz_e_o$pht1)
table(phdatmz_e_o$pht2)
#
table(phdatdz_e_o$pht1)
table(phdatdz_e_o$pht2)
#
table(phdatmz_e_o$pht1,phdatmz_e_o$pht2)
table(phdatdz_e_o$pht1,phdatdz_e_o$pht2)
#
#head(phdatdz_e)
#head(phdatmz_e)
#
phdatmz_e_o[, 'pht1'] = umxFactor(phdatmz_e_o[, 'pht1'])
phdatmz_e_o[, 'pht2'] = umxFactor(phdatmz_e_o[, 'pht2'])
phdatdz_e_o[, 'pht1'] = umxFactor(phdatdz_e_o[, 'pht1'])
phdatdz_e_o[, 'pht2'] = umxFactor(phdatdz_e_o[, 'pht2'])
#
PHENO_M2=umxACEv(name='PHENO_AC_DE',selDVs,selCovs=selCovs,dzCr = 1,dzData=phdatdz_e_o, mzData=phdatmz_e_o, sep="") 
#
##
# Organize data in long format simulated data long format or vertical format
# 
# Long format exact simulated data
phdatmzL_e = matrix(1,nmz*2,4)
phdatmzL_e[,2]=nmz + c(c(1:nmz),c(1:nmz))
phdatmzL_e[,3]=c(phdatmz_e$pht1,phdatmz_e$pht2)
phdatmzL_e[,4]=c(phdatmz_e$pgst1,phdatmz_e$pgst2)
#
ix_ = sort.int(phdatmzL_e[,2], index.return=T)
phdatmzL_e = phdatmzL_e[ix_$ix,]
colnames(phdatmzL_e)=c("zyg","famnr","ph","pgst")
phdatmzL_e=as.data.frame(phdatmzL_e)
#
#
# Long format exact simulated data
#
phdatdzL_e = matrix(2,ndz*2,4)
phdatdzL_e[,2]=c(c(1:ndz),c(1:ndz))
phdatdzL_e[,3]=c(phdatdz_e$pht1,phdatdz_e$pht2)
phdatdzL_e[,4]=c(phdatdz_e$pgst1,phdatdz_e$pgst2)

#
ix_ = sort.int(phdatdzL_e[,2], index.return=T)
phdatdzL_e = phdatdzL_e[ix_$ix,]
colnames(phdatdzL_e)=c("zyg","famnr","ph","pgst")
phdatdzL_e=as.data.frame(phdatdzL_e)
#
phdatL_e=rbind(phdatmzL_e, phdatdzL_e)
# 
# --------------------------------------------------- end log to wide reformat
# start the regression analysis analyses 
#
#
egeeM0mzdzL=geeglm(ph~pgst, id=famnr, corstr=cmethod,data=phdatL_e)#)$coefficients #   
#
summary(egeeM0mzdzL)
#
#
nresp=8
#          0,  1, 2,  3,   4,   5,   6,    7
presp0=c( .6,.15,.1, .05, .04, .035, .03, .02)
presp0 = presp0/sum(presp0) # should add up to 1
cp=rep(0,nresp)
for (i in 1:nresp) {
  for (j in 1:i) {
    cp[i]=cp[i]+presp0[j]
  }}
#                           0  1  2   3   4   
qs=quantile(phdatL_e$ph, probs=cp, digits = 12)
qs=round(qs,2)
phdatL_e1 = phdatL_e
phdatL_e1$ph=0  # 
for (i in 1:nresp) {
  sel=phdatL_e$ph>=qs[i]
  phdatL_e1$ph[sel]=(i)
}
table(phdatL_e1$ph)
#
# plot(phdatL_e1$ph,phdatL_e$ph)



# --------------------------------------------------------------------------------------

# --- we now have the simulation data
# Next we see if the residuals of a linear mixed model are normal or not.

library(lme4)        
library(dplyr)
library(performance)  # check_model(), the residual diagnostics
library(datawizard)   # skewness() / kurtosis()
library(ordinal)     

phdatL_e1$ph_continuous = phdatL_e$ph
phdatL_e1$famnr = factor(phdatL_e1$famnr)
phdatL_e1$zyg   = factor(phdatL_e1$zyg, levels = c(1, 2), labels = c('MZ', 'DZ'))

# ordinalise the skewed score into the three levels used in the real analyses: 0, 1-3, 4+.
phdatL_e1 = phdatL_e1 %>%
  mutate(ph_ordinal = cut(ph,
                          breaks = c(-Inf, 1, 4, Inf),
                          labels = c('no', 'low', 'high'),
                          right = FALSE,  # so a 1 becomes low and not no
                          ordered_result = TRUE))


# plot the phenotype before and after discretizing, and the pgs
# this was AI generated
par(mfrow = c(1, 3))
hist(phdatL_e1$ph_continuous, breaks = 40,
     main = 'Continuous phenotype (normal)', xlab = 'ph_continuous')
hist(phdatL_e1$ph, breaks = seq(-0.5, max(phdatL_e1$ph) + 0.5),
     main = 'Discretized phenotype (skewed)', xlab = 'ph')
hist(phdatL_e1$pgst, breaks = 40, main = 'PGS', xlab = 'pgst')
par(mfrow = c(1, 1))

# --- THE MODELS ---

fit_skewed = lmer(ph ~ pgst + (1 | famnr), data = phdatL_e1)
fit_reference = lmer(ph_continuous ~ pgst + (1 | famnr), data = phdatL_e1)

summary(fit_skewed)

# using the see package to check residuals,, posterior predictive check, homogeneity of variance
diag_checks = c('pp_check', 'qq', 'homogeneity')

diag_skewed    = check_model(fit_skewed,    check = diag_checks, detrend = FALSE)
diag_reference = check_model(fit_reference, check = diag_checks, detrend = FALSE)

diag_skewed     # skewed phenotype: the model cannot reproduce the outcome
diag_reference  # continuous phenotype: the same model behaves as it should

ggsave('results/figures/residual_diagnostics_skewed.png',    plot(diag_skewed),    width = 8, height = 5, dpi = 120)
ggsave('results/figures/residual_diagnostics_reference.png', plot(diag_reference), width = 8, height = 5, dpi = 120)

# some other tests
check_normality(fit_skewed)
check_heteroscedasticity(fit_skewed)

res_skewed    = residuals(fit_skewed,    scaled = TRUE)

skewness(res_skewed)
kurtosis(res_skewed)

# --- ordinal modelling ---

# clmm() models the ordered categories directly, so it makes no normality assumption about
# the outcome. It shows the the pgs effect on the log-odds scale.
fit_ordinal = clmm(ph_ordinal ~ pgst + (1 | famnr), data = phdatL_e1,
                   link = 'logit', threshold = 'flexible')

summary(fit_ordinal)


# Conclusion: the residuals of the linear mixed model on the skewed phenotype are clearly not normally distributed.
# The qq plot shows heavy skew. The skew and kurtosis values of the phenotype are high.
# We decide to use ordinal modelling for the main analyses