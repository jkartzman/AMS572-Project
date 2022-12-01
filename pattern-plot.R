library(mice)

mcarData1 <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/test1-mcar.csv"),header =TRUE)
mnarData1 <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/test1-mnar.csv"),header =TRUE)
mcarData2 <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/test2-mcar.csv"),header =TRUE)
mnarData2 <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/test2-mnar.csv"),header =TRUE)

md.pattern(mcarData1[c("X.bu.","X.dm.")])
md.pattern(mnarData1[c("X.bu.","X.dm.")])
md.pattern(mcarData2[c("X.hemo.","X.rbcc.","X.class.")])
md.pattern(mnarData2[c("X.hemo.","X.rbcc.","X.class.")])
