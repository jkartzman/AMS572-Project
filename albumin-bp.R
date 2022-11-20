library(dplyr)

data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"))
data

albumin_bp <- data[c("X.al.","X.bp.")]
albumin_bp$X.al. <- as.numeric(albumin_bp$X.al.)
albumin_bp$X.bp. <- as.numeric(albumin_bp$X.bp.)

albumin_bp <- na.omit(albumin_bp)
alGroups = albumin_bp %>% group_by(albumin_bp$X.al.)
albuminGroups <- group_split(alGroups)

albuminVsBP <- list()

albumin <- list(0,1,2,3,4,5)

for(i in 0:5){
  print(albumin_bp$X.bp.[which(albumin_bp$X.al.==i)])
  albuminVsBP <- append(albuminVsBP,list(albumin_bp$X.bp.[which(albumin_bp$X.al.==i)]))
}
boxplot(albuminVsBP,main="Albumin vs Blood Pressure Levels", xlab='Albumin', ylab='Blood Pressure',names=c(0,1,2,3,4,5))
