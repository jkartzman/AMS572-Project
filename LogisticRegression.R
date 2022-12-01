library(mice)
library(MASS)
cols <- c("numeric","numeric","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","factor","factor","factor")
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"),header = TRUE,colClasses = cols,na.strings = "?")

originalData <- data

#Converting ckd to binary numbers
levels(data$X.class.) <- gsub("\t","",levels(data$X.class.))
levels(data$X.class.) <- gsub("notckd","0",levels(data$X.class.))
levels(data$X.class.) <- gsub("ckd","1",levels(data$X.class.))
data$X.class. <- as.numeric(as.character(data$X.class.))

#Converting cad to binary numbers
levels(data$X.cad.) <- gsub("\t","",levels(data$X.cad.))
levels(data$X.cad.) <- gsub("no","0",levels(data$X.cad.))
levels(data$X.cad.) <- gsub("yes","1",levels(data$X.cad.))
levels(completeData$X.cad.) <- gsub("\tno","0",levels(completeData$X.cad.))
data$X.cad. <- as.numeric(as.character(data$X.cad.))

#Converting dm to binary numbers
levels(data$X.dm.) <- gsub(" ","",levels(data$X.dm.))
levels(data$X.dm.) <- gsub("\t","",levels(data$X.dm.))
levels(data$X.dm.) <- gsub("no","0",levels(data$X.dm.))
levels(data$X.dm.) <- gsub("yes","1",levels(data$X.dm.))
data$X.dm. <- as.numeric(as.character(data$X.dm.))

#Converting appetite to binary numbers
levels(data$X.appet.) <- gsub("good","1",levels(data$X.appet.))
levels(data$X.appet.) <- gsub("poor","0",levels(data$X.appet.))
data$X.appet. <- as.numeric(as.character(data$X.appet.))

#Converting pedal edema to binary numbers
levels(data$X.pe.) <- gsub("yes","1",levels(data$X.pe.))
levels(data$X.pe.) <- gsub("no","0",levels(data$X.pe.))
data$X.pe. <- as.numeric(as.character(data$X.pe.))

#Converting anemia to binary numbers
levels(data$X.ane.) <- gsub("yes","1",levels(data$X.ane.))
levels(data$X.ane.) <- gsub("no","0",levels(data$X.ane.))
data$X.ane. <- as.numeric(as.character(data$X.ane.))

#Converting Hypertension to binary numbers
levels(data$X.htn.) <- gsub("yes","1",levels(data$X.htn.))
levels(data$X.htn.) <- gsub("no","0",levels(data$X.htn.))
data$X.htn. <- as.numeric(as.character(data$X.htn.))

#Converting rbc to binary numbers
levels(data$X.rbc.) <- gsub("abnormal","0",levels(data$X.rbc.))
levels(data$X.rbc.) <- gsub("normal","1",levels(data$X.rbc.))
data$X.rbc. <- as.numeric(as.character(data$X.rbc.))

#Converting pc to binary numbers
levels(data$X.pc.) <- gsub("abnormal","0",levels(data$X.pc.))
levels(data$X.pc.) <- gsub("normal","1",levels(data$X.pc.))
data$X.pc. <- as.numeric(as.character(data$X.pc.))

#Converting pcc to binary numbers
levels(data$X.pcc.) <- gsub("notpresent","0",levels(data$X.pcc.))
levels(data$X.pcc.) <- gsub("present","1",levels(data$X.pcc.))
data$X.pcc. <- as.numeric(as.character(data$X.pcc.))

#Converting ba to binary numbers
levels(data$X.ba.) <- gsub("notpresent","0",levels(data$X.ba.))
levels(data$X.ba.) <- gsub("present","1",levels(data$X.ba.))
data$X.ba. <- as.numeric(as.character(data$X.ba.))

#removing rows with missing data in any one column
completeData <- na.omit(data)

sprintf("Number of rows after removing rows with at least 1 missing value is : %d",nrow(completeData))

#Now complete data has all the rows with all entries present. Rows in original data
#with at least one value missing are removed

#complete data has 158 data points


# Method to get a glm model based on data and formula
getLogisticRegressionModel <- function(formula,data) {
  model <- glm(formula,data = data, family='binomial')
  return(model)
}


formulaSelectiveFactors = X.class. ~ X.age.+ X.bp.+ X.pot.+ X.sod.+ X.appet.+ X.wbcc.+ X.bgr.+ X.hemo.+ X.pcv. + X.rbcc.

completeDataModel <- glm(formulaSelectiveFactors, completeData , family='binomial')
summary(completeDataModel)

stepAICCompleteDataModel <- stepAIC(completeDataModel, direction = "both")
summary(stepAICCompleteDataModel)


# Filling in missing values in original data
imputedData <- mice(data,m=5,maxit=5,seed=500)
imputedData <- complete(imputedData)

sprintf("Number of rows in imputed data set is : %d",nrow(imputedData))

imputedDataModel <- glm(formulaSelectiveFactors, imputedData , family='binomial')
summary(imputedDataModel)
imputedDataModel$coefficients

stepAICImputedDataModel <- stepAIC(imputedDataModel, direction = "both")
summary(stepAICImputedDataModel)
stepAICImputedDataModel$coefficients


#Splitting Data into training and testing
set.seed(1)

split <- sample(c(TRUE,FALSE),nrow(imputedData),replace = TRUE,prob=c(0.7,0.3))

trainData <- imputedData[split,]
testData <- imputedData[!split,]

trainModel <- glm(formulaSelectiveFactors, trainData , family='binomial')
stepAICTrainModel <- stepAIC(trainModel, direction = "both")
summary(stepAICTrainModel)

nullModel <- glm(X.class. ~ 1, trainData , family='binomial')
summary(nullModel)

anova(nullModel,stepAICTrainModel)

predictions <- predict(stepAICTrainModel,testData,type = "response")

predictions <- ifelse(predictions>=0.5,1,0)
testData$X.class.
predictions
correct <- 0

for( x in 1:nrow(testData)){
  if(testData$X.class.[x]==predictions[x]){
    correct = correct+1
  }
}
correct
p_value=1-pchisq(382.08-72.50,6)
sprintf("The p-value for purpose of statistical inference is : %f",p_value)
accuracy=correct/nrow(testData)
sprintf("The accuracy of the model as calculated on the test data is : %f",accuracy)



