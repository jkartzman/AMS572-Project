data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"))
data

convert_to_numeric <- function(data,useMean) {
  data <- as.numeric(data)
  meanData = mean(data,na.rm=TRUE)
  if(useMean)
    data[is.na(data)]<-meanData
  else
    data[is.na(data)]<-0
  return(data)
}

age <- convert_to_numeric(data$X.age,TRUE)


Y1 <- gsub("\t","",data$X.class.)
Y2 <- gsub("notckd","0",Y1)
Y3 <- gsub("ckd","1",Y2)
Y3

class <- convert_to_numeric(Y3,FALSE)

library(dplyr)

age_class <- data.frame(age,class)
age_class

age_ckd = age_class[1:250,1]
age_ckd

age_notCkd = age_class[251:401,1]
age_notCkd

data$X.dm.

DM1 <- gsub("yes","1",data$X.dm.)
DM2 <- gsub("no","0",DM1)
DM2

dm <- convert_to_numeric(DM2,FALSE)


boxplot(age_ckd, main="Age plot for people with Chronic Kidney Disease", ylab='Age')

boxplot(age_notCkd, main="Age plot for people without Chronic Kidney Disease", ylab='Age')

plot(age,dm)

bp <- convert_to_numeric(data$X.bp.,TRUE)

plot(age,bp)

alGroups =data %>% group_by(data$X.al.)
albuminGroups <- group_split(alGroups)

albuminGroups

albuminGroups[[7]]$X.bu.

al4 = data$X.bu.[which(data$X.al.==4)]

albuminVsBU <- list()

albumin <- list(0,1,2,3,4,5)

for(i in 0:5){
  print(data$X.bu.[which(data$X.al.==i)])
  albuminVsBU <- append(albuminVsBU,list(convert_to_numeric(data$X.bu.[which(data$X.al.==i)],TRUE)))
}
boxplot(albuminVsBU,main="Albumin vs Blood Urea Levels", xlab='Albumin', ylab='Blood Urea',names=c(0,1,2,3,4,5))