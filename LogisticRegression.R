install.packages("car")
library(MASS)
library(car)
#we load the imputed dataset with 400 rows
imputedData <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/imputed.csv"),header =TRUE)
#we load the listwise deleted dataset as completeData which only has 158 rows
completeData <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/listwise-deletion.csv"),header = TRUE)

print(sprintf("Number of rows after removing rows with at least 1 missing value is : %d",nrow(completeData)))

#Now complete data has all the rows with all entries present. Rows in original data
#with at least one value missing are removed
#complete data has 158 data points
# Method to get a glm model based on data and formula

#formula for logistic regression
formulaSelectiveFactors = X.class. ~ X.age.+ X.bp.+ X.pot.+ X.sod.+ X.appet.+ X.wbcc.+ X.bgr.+ X.hemo.+ X.pcv. + X.rbcc.
#train model on 158 row dataset and output summary, which includes residuals, AIC, and coefficients
completeDataModel <- glm(formulaSelectiveFactors, completeData , family='binomial')
print(summary(completeDataModel))
#train stepwise model to improve on old model
stepAICCompleteDataModel <- stepAIC(completeDataModel, direction = "both",trace = FALSE)
print(summary(stepAICCompleteDataModel))


print(sprintf("Number of rows in imputed data set is : %d",nrow(imputedData)))
#train model on imputed dataset with 400 rows
imputedDataModel <- glm(formulaSelectiveFactors, imputedData , family='binomial')
print(summary(imputedDataModel))

imputedDataModel$coefficients
#improve on imputed dataset with stepwise function
stepAICImputedDataModel <- stepAIC(imputedDataModel, direction = "both",trace=FALSE)
print(summary(stepAICImputedDataModel))

#Splitting Data into training and testing
set.seed(1)

split <- sample(c(TRUE,FALSE),nrow(imputedData),replace = TRUE,prob=c(0.7,0.3))
#retrain model and set up test dataset to get predictions
trainData <- imputedData[split,]
testData <- imputedData[!split,]

trainModel <- glm(formulaSelectiveFactors, trainData , family='binomial')
stepAICTrainModel <- stepAIC(trainModel, direction = "both",trace=FALSE)
print(summary(stepAICTrainModel))
#train null model to compare to original model
nullModel <- glm(X.class. ~ 1, trainData , family='binomial')
print(summary(nullModel))

anova(nullModel,stepAICTrainModel)
stepAICTrainModel$deviance
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
# collinearity assumption
print(vif(stepAICImputedDataModel))
print(vif(stepAICTrainModel))
print(sprintf("Number of correct predictions : %f",correct))
#test significance of final models 
p_value=1-pchisq(stepAICTrainModel$null.deviance-stepAICTrainModel$deviance,stepAICTrainModel$df.null-stepAICTrainModel$df.residual)
#Another approach for same calculation
p_value2=1-pchisq(nullModel$deviance-stepAICTrainModel$deviance,nullModel$df.residual-stepAICTrainModel$df.residual)
print(sprintf("The p-value for purpose of statistical inference is : %f",p_value))
accuracy=correct/nrow(testData)
print(sprintf("The accuracy of the model as calculated on the test data is : %f",accuracy))
