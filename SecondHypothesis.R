#plot for age and ckd (after removing missing values)

cols <- c("numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","factor","factor","factor")
df <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"),header = TRUE,colClasses = cols,na.strings = "?")

#removing rows with missing data in any one column
data <- na.omit(df)

#Converting ckd to binary numbers
data$X.class. <- gsub("\t","",data$X.class.)
data$X.class. <- gsub("notckd","0",data$X.class.)
data$X.class. <- gsub("ckd","1",data$X.class.)
data$X.class. <- as.numeric(data$X.class.)
data$X.class.

#Converting cad to binary numbers
data$X.cad. <- gsub("\tno","0",data$X.class.)
data$X.cad. <- gsub("no","0",data$X.class.)
data$X.cad. <- gsub("yes","1",data$X.class.)
data$X.cad. <- as.numeric(data$X.cad.)
data$X.cad.

#Converting dm to binary numbers
#data$X.cad. <- gsub("\tno","0",data$X.class.)
data$X.dm. <- gsub("no","0",data$X.dm.)
data$X.dm. <- gsub("yes","1",data$X.dm.)
data$X.dm. <- as.numeric(data$X.dm.)
data$X.dm

# Age vs Chronic Kidney Disease Box plot
boxplot(data$X.age.[which(data$X.class.==0)],data$X.age.[which(data$X.class.==1)],
        main="Age vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Age',names=c("No","Yes"))


# Blood Pressue vs Chronic Kidney Disease Box plot
boxplot(data$X.bp.[which(data$X.class.==0)],data$X.bp.[which(data$X.class.==1)],
        main="Blood Pressure vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Blood Pressure',names=c("No","Yes"))

# Potassium vs Chronic Kidney Disease Box plot
boxplot(data$X.pot.[which(data$X.class.==0)],data$X.pot.[which(data$X.class.==1)],
        main="Potassium vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Potassium',names=c("No","Yes"), ylim=c(0,10))


#Relation between coronary artery disease and chronic kidney disease
table(data$X.cad.,data$X.class.)
cor(data$X.cad.,data$X.class.)

#Relation between diabetes mellitus and chronic kidney disease
table(data$X.dm.,data$X.class.)
cor(data$X.dm.,data$X.class.)

# Hemoglobin vs Chronic Kidney Disease Box plot
boxplot(data$X.hemo.[which(data$X.class.==0)],data$X.hemo.[which(data$X.class.==1)],
        main="Hemoglobin vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Hemoglobin',names=c("No","Yes"))


#Logistic Regression

getLogisticRegressionModel <- function(formula,data) {
  model <- glm(formula,data = data, family='binomial')
  return(model)
}

getSSE <- function(model,observedValues){
  return(sum((fitted(model)-observedValues)^2))
}

shouldFactorBeIncluded <- function(sseInitial,sseFinal,n,p,alpha){
  f1 <- (sseInitial-sseFinal)/(sseFinal/(n-p+1))
  f2 <- (qf(1-alpha,1,n-p+1))
  return(f1>f2)
}

#Model with just Age as factor

age_ckd_df <- data.frame(data$X.age,data$X.class.)
ageModel <- getLogisticRegressionModel(data$X.class. ~ data$X.age.,age_ckd_df)
summary(ageModel)

sse_age <- getSSE(ageModel,data$X.class.)

sse_age

#Model with adding bp along with age as a factor

age_bp_ckd_df <- data.frame(data$X.age,data$X.bp.,data$X.class.)
ageBPModel <- getLogisticRegressionModel(data$X.class. ~ data$X.age. + data$X.bp.,age_bp_ckd_df)
summary(ageBPModel)

sse_bp_age <- getSSE(ageBPModel,data$X.class.)

sse_bp_age

alpha = 0.05

shouldBPBeIncluded <- shouldFactorBeIncluded(sse_age,sse_bp_age,nrow(data),2,alpha)

shouldBPBeIncluded

#Model with adding Potassium along with age,bp as a factor

ageBPPtm_ckd_df <- data.frame(data$X.age,data$X.bp.,data$X.pot.,data$X.class.)
ageBPPtmModel <- getLogisticRegressionModel(data$X.class. ~ data$X.age. + data$X.bp. + data$X.pot.,ageBPPtm_ckd_df)
summary(ageBPPtmModel)

sse_bp_ptm_age <- getSSE(ageBPPtmModel,data$X.class.)

sse_bp_ptm_age

shouldPotassiumBeIncluded <- shouldFactorBeIncluded(sse_bp_age,sse_bp_ptm_age,nrow(data),3,alpha)

shouldPotassiumBeIncluded

#Model with adding coronary Artery along with age,bp,potassium as a factor

cumulativeCADDF <- data.frame(data$X.age,data$X.bp.,data$X.pot.,data$X.cad.,data$X.class.)
cumulativeCADModel <- getLogisticRegressionModel(data$X.class. ~ data$X.age. + data$X.bp. + data$X.pot. + data$X.cad.,cumulativeCADDF)
summary(cumulativeCADModel)

sse_cad <- getSSE(cumulativeCADModel,data$X.class.)

sse_cad

shouldCADBeIncluded <- shouldFactorBeIncluded(sse_bp_ptm_age,sse_cad,nrow(data),4,alpha)

shouldCADBeIncluded

#Model with adding Hemoglobin along with age,bp,potassium,cad as a factor

cumulativeHemoDF <- data.frame(data$X.age,data$X.bp.,data$X.pot.,data$X.cad.,data$X.hemo.,data$X.class.)
cumulativeHemoModel <- getLogisticRegressionModel(data$X.class. ~ data$X.age. + data$X.bp. + data$X.pot. + data$X.cad. + data$X.hemo.,cumulativeCADDF)
summary(cumulativeHemoModel)

sse_hemo <- getSSE(cumulativeHemoModel,data$X.class.)

sse_hemo

shouldHemoBeIncluded <- shouldFactorBeIncluded(sse_cad,sse_hemo,nrow(data),5,alpha)

shouldHemoBeIncluded