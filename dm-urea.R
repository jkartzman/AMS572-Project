library(dplyr)

data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"))
dm_bu <- data[c("X.dm.","X.bu.")]
dm_bu$X.bu. <- as.numeric(dm_bu$X.bu.)

dm_bu$X.dm. <- gsub("\t","",dm_bu$X.dm.)

dm_bu
dm_bu <- na.omit(dm_bu)

dmGroups = dm_bu %>% group_by(dm_bu$X.dm.)
dmGroups <- group_split(dmGroups)

dmGroups


DMVsBU <- list()

dm <- list("no","yes")

for(i in dm){
  print(dm_bu$X.bu.[which(dm_bu$X.dm.==i)])
  DMVsBU <- append(DMVsBU,list(dm_bu$X.bu.[which(dm_bu$X.dm.==i)]))
}
boxplot(DMVsBU,main="Diabetes Milletus vs Blood Urea Levels", xlab='Diabetes Milletus', ylab='Blood Urea',names=c("no","yes"))

anova(lm(X.bu.~X.dm., data=dm_bu))
