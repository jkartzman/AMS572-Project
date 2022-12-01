library(mice)

# Generate MNAR for test 1:
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/imputed.csv"),header = TRUE)
mnar_dataset <- data[c("X.bu.","X.dm.")]
mnar <- ampute(mnar_dataset,prop = 0.2,patterns=c(0,1),weights=c(0,1),mech = "MNAR",type="RIGHT")
mnar_amputed <- mnar$amp
data$X.bu. <- mnar_amputed$X.bu.
data$X.dm. <- mnar_amputed$X.dm.
write.csv(data,"test1-mnar.csv",row.names=FALSE)

# Generate MCAR for test 1:
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/imputed.csv"),header = TRUE)
mcar_dataset <- data[c("X.bu.","X.dm.")]
mcar <- ampute(mcar_dataset,prop = 0.2,patterns=c(0,1),mech = "MCAR")
mcar_amputed <- mcar$amp
data$X.bu. <- mcar_amputed$X.bu.
data$X.dm. <- mcar_amputed$X.dm.
write.csv(data,"test1-mcar.csv",row.names=FALSE)

# Generate MNAR for test 2:
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/imputed.csv"),header = TRUE)
mnar_dataset <- data[c("X.rbcc.","X.hemo.","X.class.")]
patterns <- matrix(data=c(0,0,1,0,1,1,1,0,1),nrow=3,byrow = TRUE)
weights <- matrix(data=c(0,0,1,0,0,1,0,0,1),nrow=3,byrow = TRUE)
mnar <- ampute(mnar_dataset,prop = 0.2,patterns=patterns,weights=weights,mech = "MNAR",type="RIGHT")
mnar_amputed <- mnar$amp
data$X.rbcc. <- mnar_amputed$X.rbcc.
data$X.hemo. <- mnar_amputed$X.hemo.
data$X.class. <- mnar_amputed$X.class.
write.csv(data,"test2-mnar.csv",row.names=FALSE)

# Generate MCAR for test 2:
data <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/imputed.csv"),header = TRUE)
mcar_dataset <- data[c("X.rbcc.","X.hemo.","X.class.")]
mcar <- ampute(mcar_dataset,prop = 0.2,patterns=patterns,mech = "MCAR")
mcar_amputed <- mcar$amp
data$X.rbcc. <- mcar_amputed$X.rbcc.
data$X.hemo. <- mcar_amputed$X.hemo.
data$X.class. <- mcar_amputed$X.class.
write.csv(data,"test2-mcar.csv",row.names=FALSE)
