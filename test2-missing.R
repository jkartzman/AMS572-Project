library(MASS)
library(mice)
# load MCAR/MNAR datasets
mcarData <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/test2-mcar.csv"),header =TRUE)
mnarData <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/test2-mnar.csv"),header = TRUE)
# this section consists of filling missing data with multiple imputation
print("Using mice to impute:")
imp <- mice(mcarData,m=1,maxit=2,seed=500)
impMCAR <- complete(imp)
imp <- mice(mnarData,m=1,maxit=2,seed=500)
impMNAR <- complete(imp)
# load formula
formulaSelectiveFactors = X.class. ~ X.age.+ X.bp.+ X.pot.+ X.sod.+ X.appet.+ X.wbcc.+ X.bgr.+ X.hemo.+ X.pcv. + X.rbcc.
# generate glm models using MNAR/MCAR
mnarModel <- glm(formulaSelectiveFactors, impMNAR, family='binomial')
print(summary(mnarModel))
mcarModel <- glm(formulaSelectiveFactors, impMCAR, family='binomial')
print(summary(mcarModel))
# improve with stepAIC
stepAICMNAR <- stepAIC(mnarModel, direction = "both",trace=FALSE)
print(summary(stepAICMNAR))
stepAICMCAR <- stepAIC(mcarModel, direction = "both",trace=FALSE)
print(summary(stepAICMCAR))
# get significance of models
p_value_mnar=1-pchisq(stepAICMNAR$null.deviance-stepAICMNAR$deviance,stepAICMNAR$df.null-stepAICMNAR$df.residual)
p_value_mcar=1-pchisq(stepAICMCAR$null.deviance-stepAICMCAR$deviance,stepAICMCAR$df.null-stepAICMCAR$df.residual)
print(paste("P-value on imputed MCAR data: ",p_value_mcar))
print(paste("P-value on imputed MNAR data: ",p_value_mnar))
# use listwise deletion (removes rows with NAs) to deal with MCAR/MNAR data
print("Using listwise deletion:")
mcar_listwise <- na.omit(mcarData)
mnar_listwise <- na.omit(mnarData)
# train GLM model on mnar/mcar
mnarModel <- glm(formulaSelectiveFactors, mnar_listwise, family='binomial')
print(summary(mnarModel))
mcarModel <- glm(formulaSelectiveFactors, mcar_listwise, family='binomial')
print(summary(mcarModel))
# improve with stepwise
stepAICMNAR <- stepAIC(mnarModel, direction = "both",trace=FALSE)
print(summary(stepAICMNAR))
stepAICMCAR <- stepAIC(mcarModel, direction = "both",trace=FALSE)
print(summary(stepAICMCAR))
#test significance
p_value_mnar=1-pchisq(stepAICMNAR$null.deviance-stepAICMNAR$deviance,stepAICMNAR$df.null-stepAICMNAR$df.residual)
p_value_mcar=1-pchisq(stepAICMCAR$null.deviance-stepAICMCAR$deviance,stepAICMCAR$df.null-stepAICMCAR$df.residual)
print(paste("P-value on listwise deleted MCAR data: ",p_value_mcar))
print(paste("P-value on listwise MNAR data: ",p_value_mnar))

