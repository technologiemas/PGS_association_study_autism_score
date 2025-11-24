gc()
rm(list = ls(all = TRUE))

library(semTools)
library(lavaan)
# library(lavaan.mi) 

data = readRDS("data/processed/02_data_all_items.rds")

model_mother = 'autism_score =~ q1m12 + q9m12 + q17m12 + q42m12 + q62m12 + q66m12 + q79m12 + q80m12 + q84m12 + q111m12'
# model_father = 'autism_score =~ q1v12 + q9v12 + q17v12 + q42v12 + q62v12 + q66v12 + q79v12 + q80v12 + q84v12 + q111v12'
# model_teacher = 'autism_score =~ q1t12 + q9t12 + q17t12 + q42t12 + q62t12 + q66t12 + q79t12 + q80t12 + q84t12 + q111t12'
model_self = 'autism_score =~ q1ysr14 + q9ysr14 + q17ysr14 + q42ysr14 + q62ysr14 + q66ysr14 + q79ysr14 + q84ysr14 + q111ysr14'


run_measurement_invariance = function(model) {
    cfa_1 = cfa(model, data = data, estimator = "MLR", cluster = "FamilyNumber")
    # print(summary(cfa_1, fit.measures = TRUE, standardized = TRUE))

    cfa_config = cfa(model, data = data, group = "sex", cluster = "FamilyNumber", estimator = "MLR")
    cfa_metric = cfa(model, data = data, group = "sex", group.equal = "loadings", cluster = "FamilyNumber", estimator = "MLR")
    cfa_scalar = cfa(model, data = data, group = "sex", group.equal = c("loadings","intercepts"), cluster = "FamilyNumber", estimator = "MLR")
    # cfa.strict = cfa(model, data = data, group = "sex", group.equal = c("loadings","intercepts","residuals"), cluster = "FamilyNumber", estimator = "MLR")

    # print("Summaries of the models:")
    print(summary(cfa_config, fit.measures = TRUE, standardized = TRUE))
    # summary(cfa.metric, fit.measures = TRUE, standardized = TRUE)
    # summary(cfa.strict, fit.measures = TRUE, standardized = TRUE)
    # summary(cfa.scalar, fit.measures = TRUE, standardized = TRUE

    print("Comparisons:")
    print("Configural vs Metric")
    compfit1 = semTools::compareFit(cfa_config, cfa_metric)
    summary(compfit1)

    print("Metric vs Scalar")
    compfit2 = semTools::compareFit(cfa_metric, cfa_scalar)
    summary(compfit2)

    # print("Scalar vs Strict")
    # compfit3 = semTools::compareFit(cfa.scalar, cfa.strict)
    # summary(compfit3)

    print("Which items differ between the groups male and female:")
    lavTestScore(cfa_metric)
}

run_measurement_invariance(model_mother)
run_measurement_invariance(model_self)

cfa_males = cfa(model_mother, data = data[data$sex == "MALE", ], estimator = "MLR", cluster = "FamilyNumber")
cfa_females = cfa(model_mother, data = data[data$sex == "FEMALE", ], estimator = "MLR", cluster = "FamilyNumber")
summary(cfa_males)
summary(cfa_females)
compfit = semTools::compareFit(cfa_males, cfa_females)
summary(compfit)

# notes: now the MLR assumes continuous data while mine is ordinal (0, 1, 2). So the results may not be accurate. Need to find a way to do this with WLSMV or DWLS. MPlus seems to be able to do this.