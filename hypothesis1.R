cols <- c("numeric","numeric","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","factor","factor","factor")
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"),header = TRUE,colClasses = cols,na.strings = "?")
levels(data$X.dm.) <- gsub(" ","",gsub("\t","",levels(data$X.dm.)))

boxplot(data$X.bu.[which(data$X.dm.=="no")],data$X.bu.[which(data$X.dm.=="yes")],
        main="Diabetes Mellitus vs Blood Urea Levels (non-imputed)", xlab='Diabetes Mellitus', 
        ylab='Blood Urea',names=c("no","yes"))
bu1 <- na.omit(data$X.bu.[which(data$X.dm.=="no")])
bu2 <- na.omit(data$X.bu.[which(data$X.dm.=="yes")])
test_statistic <- (mean(bu1)-mean(bu2))/sqrt(sd(bu1)^2/length(bu1)+sd(bu2)^2/length(bu2))
print(paste("P-value of original: ",2*(1-pnorm(abs(test_statistic)))))

# imputed version
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/imputed.csv"),header = TRUE)
boxplot(data$X.bu.[which(data$X.dm.==0)],data$X.bu.[which(data$X.dm.==1)],
        main="Diabetes Mellitus vs Blood Urea Levels (imputed)", xlab='Diabetes Milletus',
        ylab='Blood Urea',names=c("no","yes"))
bu1 <- data$X.bu.[which(data$X.dm.==0)]
bu2 <- data$X.bu.[which(data$X.dm.==1)]
test_statistic <- (mean(bu1)-mean(bu2))/sqrt(sd(bu1)^2/length(bu1)+sd(bu2)^2/length(bu2))
print(paste("P-value after imputation: ",2*(1-pnorm(abs(test_statistic)))))
