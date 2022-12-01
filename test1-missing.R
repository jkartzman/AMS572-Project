library(mice)

# MNAR:
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/test1-mnar.csv"),header=TRUE)
mnar_amputed <- data[c("X.bu.","X.dm.")]
print(paste("MNAR: Number of non-diabetes patients with missing values: ",sum(mnar_amputed$X.dm.[is.na(mnar_amputed$X.bu.)]==0)))
print(paste("MNAR: Number of diabetes patients with missing values: ",sum(mnar_amputed$X.dm.[is.na(mnar_amputed$X.bu.)]==1)))
#Listwise deletion:
mnar_na <- na.omit(mnar_amputed)
boxplot(mnar_na$X.bu.[which(mnar_na$X.dm.==0)],mnar_na$X.bu.[which(mnar_na$X.dm.==1)],
        main="Diabetes Mellitus vs Blood Urea Levels (MNAR)", xlab='Diabetes Mellitus',
        ylab='Blood Urea',names=c("no","yes"))
mnar_bu1 <- mnar_na$X.bu.[which(mnar_na$X.dm.==0)]
mnar_bu2 <- mnar_na$X.bu.[which(mnar_na$X.dm.==1)]

test_statistic <- (mean(mnar_bu1)-mean(mnar_bu2))/sqrt(sd(mnar_bu1)^2/length(mnar_bu1)+sd(mnar_bu2)^2/length(mnar_bu2))
print(paste("MNAR P-value after listwise deletion: ",2*(1-pnorm(abs(test_statistic)))))

# Use mice imputation
imp <- mice(data,m=1,maxit=2,seed=500,printFlag = FALSE)
data <- complete(imp)
mnar_imp <- data[c("X.bu.","X.dm.")]
boxplot(mnar_imp$X.bu.[which(mnar_imp$X.dm.==0)],mnar_imp$X.bu.[which(mnar_imp$X.dm.==1)],
        main="Diabetes Mellitus vs Blood Urea Levels (MNAR plus Impute)", xlab='Diabetes Mellitus',
        ylab='Blood Urea',names=c("no","yes"))
mnar_bu1 <- mnar_imp$X.bu.[which(mnar_imp$X.dm.==0)]
mnar_bu2 <- mnar_imp$X.bu.[which(mnar_imp$X.dm.==1)]

test_statistic <- (mean(mnar_bu1)-mean(mnar_bu2))/sqrt(sd(mnar_bu1)^2/length(mnar_bu1)+sd(mnar_bu2)^2/length(mnar_bu2))
print(paste("MNAR P-value after imputation: ",2*(1-pnorm(abs(test_statistic)))))


# MCAR:
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/test1-mcar.csv"),header=TRUE)
mcar_amputed <- data[c("X.bu.","X.dm.")]
print(paste("mcar: Number of non-diabetes patients with missing values: ",sum(mcar_amputed$X.dm.[is.na(mcar_amputed$X.bu.)]==0)))
print(paste("mcar: Number of diabetes patients with missing values: ",sum(mcar_amputed$X.dm.[is.na(mcar_amputed$X.bu.)]==1)))
#Listwise deletion:
mcar_na <- na.omit(mcar_amputed)
boxplot(mcar_na$X.bu.[which(mcar_na$X.dm.==0)],mcar_na$X.bu.[which(mcar_na$X.dm.==1)],
        main="Diabetes Mellitus vs Blood Urea Levels (mcar)", xlab='Diabetes Mellitus',
        ylab='Blood Urea',names=c("no","yes"))
mcar_bu1 <- mcar_na$X.bu.[which(mcar_na$X.dm.==0)]
mcar_bu2 <- mcar_na$X.bu.[which(mcar_na$X.dm.==1)]

test_statistic <- (mean(mcar_bu1)-mean(mcar_bu2))/sqrt(sd(mcar_bu1)^2/length(mcar_bu1)+sd(mcar_bu2)^2/length(mcar_bu2))
print(paste("mcar P-value after listwise deletion: ",2*(1-pnorm(abs(test_statistic)))))

# Use mice imputation
imp <- mice(data,m=1,maxit=2,seed=500,printFlag = FALSE)
data <- complete(imp)
mcar_imp <- data[c("X.bu.","X.dm.")]
boxplot(mcar_imp$X.bu.[which(mcar_imp$X.dm.==0)],mcar_imp$X.bu.[which(mcar_imp$X.dm.==1)],
        main="Diabetes Mellitus vs Blood Urea Levels (mcar plus Impute)", xlab='Diabetes Mellitus',
        ylab='Blood Urea',names=c("no","yes"))
mcar_bu1 <- mcar_imp$X.bu.[which(mcar_imp$X.dm.==0)]
mcar_bu2 <- mcar_imp$X.bu.[which(mcar_imp$X.dm.==1)]

test_statistic <- (mean(mcar_bu1)-mean(mcar_bu2))/sqrt(sd(mcar_bu1)^2/length(mcar_bu1)+sd(mcar_bu2)^2/length(mcar_bu2))
print(paste("mcar P-value after imputation: ",2*(1-pnorm(abs(test_statistic)))))
