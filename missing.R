library(mice)

#load data
cols <- c("numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","factor","factor","factor")
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"),header = TRUE,colClasses = cols,na.strings = "?")
levels(data$X.dm.) <- gsub(" ","",gsub("\t","",levels(data$X.dm.)))
levels(data$X.class.) <- gsub(" ","",gsub("\t","",levels(data$X.class.)))
levels(data$X.cad.) <- gsub(" ","",gsub("\t","",levels(data$X.cad.)))

imp <- mice(data,m=5,maxit=5,seed=500)
imputed <- complete(imp)
print(imp$method)
#md.pattern(data)
