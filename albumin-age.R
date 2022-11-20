library(dplyr)

data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"))
data

albumin_age <- data[c("X.al.","X.age.")]
albumin_age$X.al. <- as.numeric(albumin_age$X.al.)
albumin_age$X.age. <- as.numeric(albumin_age$X.age.)

albumin_age <- na.omit(albumin_age)
alGroups = albumin_age %>% group_by(albumin_age$X.al.)
albuminGroups <- group_split(alGroups)

albuminVsage <- list()

albumin <- list(0,1,2,3,4,5)

for(i in 0:5){
  print(albumin_age$X.age.[which(albumin_age$X.al.==i)])
  albuminVsage <- append(albuminVsage,list(albumin_age$X.age.[which(albumin_age$X.al.==i)]))
}
boxplot(albuminVsage,main="Albumin vs age Levels", xlab='Albumin', ylab='age',names=c(0,1,2,3,4,5))
