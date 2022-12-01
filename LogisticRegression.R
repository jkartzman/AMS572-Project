library(mice)
library(MASS)
completeData <- read.csv("listwise-deletion.csv",header = TRUE)
imputeData <- read.csv("imputed.csv",header = TRUE)

sprintf("Number of rows after removing rows with at least 1 missing value is : %d",nrow(completeData))

#Now complete data has all the rows with all entries present. Rows in original data
#with at least one value missing are removed
#complete data has 158 data points
# Method to get a glm model based on data and formula

formulaSelectiveFactors = X.class. ~ X.age.+ X.bp.+ X.pot.+ X.sod.+ X.appet.+ X.wbcc.+ X.bgr.+ X.hemo.+ X.pcv. + X.rbcc.

completeDataModel <- glm(formulaSelectiveFactors, completeData , family='binomial')
print(summary(completeDataModel))

stepAICCompleteDataModel <- stepAIC(completeDataModel, direction = "both",trace = FALSE)
print(summary(stepAICCompleteDataModel))


sprintf("Number of rows in imputed data set is : %d",nrow(imputedData))

imputedDataModel <- glm(formulaSelectiveFactors, imputedData , family='binomial')
print(summary(imputedDataModel))

stepAICImputedDataModel <- stepAIC(imputedDataModel, direction = "both",trace=FALSE)
print(summary(stepAICImputedDataModel))

#Splitting Data into training and testing
set.seed(1)

split <- sample(c(TRUE,FALSE),nrow(imputedData),replace = TRUE,prob=c(0.7,0.3))

trainData <- imputedData[split,]
testData <- imputedData[!split,]

trainModel <- glm(formulaSelectiveFactors, trainData , family='binomial')
stepAICTrainModel <- stepAIC(trainModel, direction = "both",trace=FALSE)
print(summary(stepAICTrainModel))

nullModel <- glm(X.class. ~ 1, trainData , family='binomial')
print(summary(nullModel))

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
sprintf("Number of correct predictions : %f",correct)
p_value=1-pchisq(382.08-74.15,7)
sprintf("The p-value for purpose of statistical inference is : %f",p_value)
accuracy=correct/nrow(testData)
sprintf("The accuracy of the model as calculated on the test data is : %f",accuracy)
