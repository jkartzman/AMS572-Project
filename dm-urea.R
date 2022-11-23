library(dplyr)

cols <- c("numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","factor","factor","factor")
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"),header = TRUE,colClasses = cols,na.strings = "?")
levels(data$X.dm.) <- gsub(" ","",gsub("\t","",levels(data$X.dm.)))

boxplot(data$X.bu.[which(data$X.dm.=="no")],data$X.bu.[which(data$X.dm.=="yes")],
        main="Diabetes Milletus vs Blood Urea Levels", xlab='Diabetes Milletus', 
        ylab='Blood Urea',names=c("no","yes"))
bu1 <- na.omit(data$X.bu.[which(data$X.dm.=="no")])
bu2 <- na.omit(data$X.bu.[which(data$X.dm.=="yes")])
x <- c(rep(mean(bu1),length(bu1)),rep(mean(bu2),length(bu2)))
res <- c(bu1-mean(bu1),bu2-mean(bu2))
plot(x,res,xlab="fitted values",ylab="residuals")
abline(0,0)
plot(qnorm((1:length(res))/(length(res)+1)),sort(res))
#print(anova(lm(X.bu.~X.dm.,data = data)))
test_statistic <- (mean(bu1)-mean(bu2))/sqrt(sd(bu1)^2/length(bu1)+sd(bu2)^2/length(bu2))
print(paste("P-value: ",2*(1-pnorm(abs(test_statistic)))))
