library(leaps)
library(bestglm)

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
data$X.cad. <- gsub("\tno","0",data$X.cad.)
data$X.cad. <- gsub("no","0",data$X.cad.)
data$X.cad. <- gsub("yes","1",data$X.cad.)
data$X.cad. <- as.numeric(data$X.cad.)
data$X.cad.

#Converting dm to binary numbers
#data$X.cad. <- gsub("\tno","0",data$X.class.)
data$X.dm. <- gsub("no","0",data$X.dm.)
data$X.dm. <- gsub("yes","1",data$X.dm.)
data$X.dm. <- as.numeric(data$X.dm.)
data$X.dm

#Converting anemia to binary numbers
data$X.ane. <- gsub("no","0",data$X.ane.)
data$X.ane. <- gsub("yes","1",data$X.ane.)
data$X.ane. <- as.numeric(data$X.ane.)
data$X.ane

#Converting anemia to binary numbers
data$X.pe. <- gsub("no","0",data$X.pe.)
data$X.pe. <- gsub("yes","1",data$X.pe.)
data$X.pe. <- as.numeric(data$X.pe.)
data$X.pe

#Converting rbc to binary numbers
data$X.rbc. <- gsub("abnormal","0",data$X.rbc.)
data$X.rbc. <- gsub("normal","1",data$X.rbc.)
data$X.rbc. <- as.numeric(data$X.rbc.)
data$X.rbc


#Converting hypertension to binary numbers
data$X.htn. <- gsub("no","0",data$X.htn.)
data$X.htn. <- gsub("yes","1",data$X.htn.)
data$X.htn. <- as.numeric(data$X.htn.)
data$X.htn

#Converting appetite to binary numbers
data$X.appet. <- gsub("poor","0",data$X.appet.)
data$X.appet. <- gsub("good","1",data$X.appet.)
data$X.appet. <- as.numeric(data$X.appet.)
data$X.appet.

cor(data$X.bp.,data$X.class.)
cor(data$X.pe.,data$X.class.)
cor(data$X.hemo.,data$X.class.)
cor(data$X.age.,data$X.class.)
cor(data$X.bu.,data$X.class.)
cor(data$X.cad.,data$X.class.)
cor(data$X.dm.,data$X.class.)
cor(data$X.appet.,data$X.class.)
cor(data$X.bgr.,data$X.class.)

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



#Splitting Data into training and testing
set.seed(1)

split <- sample(c(TRUE,FALSE),nrow(data),replace = TRUE,prob=c(0.7,0.3))

trainData <- data[split,]
testData <- data[!split,]

selectiveFactorsDF <- data.frame(trainData$X.age,trainData$X.bp.,trainData$X.pot.,trainData$X.sod,trainData$X.appet.,trainData$X.wbcc.,trainData$X.bgr.,trainData$X.hemo.,trainData$X.pcv.,trainData$X.class.)

bestModels <- bestglm(selectiveFactorsDF,IC = "AIC")
bestModel <- bestModels$BestModel
summary(bestModel)

qqnorm(bestModel$residuals,xlab = "Residuals",ylab = "Normal Scores", main = "Normal Plot of Residuals for Chronic Kidney Disease Factors Data")

predictions <- predict(bestModel,testData,type = "response")
