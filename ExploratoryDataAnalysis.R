cols <- c("numeric","numeric","factor","factor","factor","factor","factor","factor","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","factor","factor","factor","factor","factor","factor","factor")
df <- read.csv(url("https://raw.githubusercontent.com/jkartzman/AMS572-Project/main/chronic_kidney_disease.csv"),header = TRUE,colClasses = cols,na.strings = "?")

#removing rows with missing data in any one column
data <- na.omit(df)

#Converting ckd to binary numbers
data$X.class. <- gsub("\t","",data$X.class.)
data$X.class. <- gsub("notckd","0",data$X.class.)
data$X.class. <- gsub("ckd","1",data$X.class.)
data$X.class. <- as.numeric(data$X.class.)
data$X.class.

#Converting cad to binary numbers
data$X.cad. <- gsub("\tno","0",data$X.cad.)
data$X.cad. <- gsub("no","0",data$X.cad.)
data$X.cad. <- gsub("yes","1",data$X.cad.)
data$X.cad. <- as.numeric(data$X.cad.)
data$X.cad.

#Converting dm to binary numbers
#data$X.cad. <- gsub("\tno","0",data$X.class.)
data$X.dm. <- gsub("no","0",data$X.dm.)
data$X.dm. <- gsub("yes","1",data$X.dm.)
data$X.dm. <- as.numeric(data$X.dm.)
data$X.dm

#Converting anemia to binary numbers
data$X.ane. <- gsub("no","0",data$X.ane.)
data$X.ane. <- gsub("yes","1",data$X.ane.)
data$X.ane. <- as.numeric(data$X.ane.)
data$X.ane

#Converting anemia to binary numbers
data$X.pe. <- gsub("no","0",data$X.pe.)
data$X.pe. <- gsub("yes","1",data$X.pe.)
data$X.pe. <- as.numeric(data$X.pe.)
data$X.pe

#Converting rbc to binary numbers
data$X.rbc. <- gsub("abnormal","0",data$X.rbc.)
data$X.rbc. <- gsub("normal","1",data$X.rbc.)
data$X.rbc. <- as.numeric(data$X.rbc.)
data$X.rbc


#Converting hypertension to binary numbers
data$X.htn. <- gsub("no","0",data$X.htn.)
data$X.htn. <- gsub("yes","1",data$X.htn.)
data$X.htn. <- as.numeric(data$X.htn.)
data$X.htn


# Age vs Chronic Kidney Disease Box plot
boxplot(data$X.age.[which(data$X.class.==0)],data$X.age.[which(data$X.class.==1)],
        main="Age vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Age',names=c("No","Yes"))


# Blood Pressue vs Chronic Kidney Disease Box plot
boxplot(data$X.bp.[which(data$X.class.==0)],data$X.bp.[which(data$X.class.==1)],
        main="Blood Pressure vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Blood Pressure',names=c("No","Yes"))

# Potassium vs Chronic Kidney Disease Box plot
boxplot(data$X.pot.[which(data$X.class.==0)],data$X.pot.[which(data$X.class.==1)],
        main="Potassium vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Potassium',names=c("No","Yes"), ylim=c(0,10))


#Relation between coronary artery disease and chronic kidney disease
table(data$X.cad.,data$X.class.)
cor(data$X.cad.,data$X.class.)

#Relation between diabetes mellitus and chronic kidney disease
table(data$X.dm.,data$X.class.)
cor(data$X.dm.,data$X.class.)

# Hemoglobin vs Chronic Kidney Disease Box plot
boxplot(data$X.hemo.[which(data$X.class.==0)],data$X.hemo.[which(data$X.class.==1)],
        main="Hemoglobin vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Hemoglobin',names=c("No","Yes"))


#Hemoglobin and Anemia relation
table(data$X.hemo.,data$X.ane.)
cor(data$X.hemo.,data$X.ane.)

#Hemoglobin and RBC relation
table(data$X.hemo.,data$X.rbc.)
cor(data$X.hemo.,data$X.rbc.)

#Hypertension and Blood pressure
table(data$X.htn.,data$X.bp.)
cor(data$X.htn.,data$X.bp.)

# red blood cells and hemoglobin count
plot(data$X.rbcc.,data$X.hemo.,xlab = "Red blood cells count", ylab = "Hemoglobin")
cor(data$X.rbcc.,data$X.hemo.)

#Relation between Pedal enema and chronic kidney disease
table(data$X.pe.,data$X.class.)
cor(data$X.pe.,data$X.class.)

#Serum Creatinine vs Chronic Kidney Disease Box plot
boxplot(data$X.sc.[which(data$X.class.==0)],data$X.sc.[which(data$X.class.==1)],
        main="Serum Creatinine vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Serum Creatinine',names=c("No","Yes"))

#White Blood Cells vs Chronic Kidney Disease Box plot
boxplot(data$X.wbcc.[which(data$X.class.==0)],data$X.wbcc.[which(data$X.class.==1)],
        main="White Blood Cells Count vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='White Blood Cells Count',names=c("No","Yes"))


#Relation between Blood Glucose Random and Diabetes Mellitus
table(data$X.bgr.,data$X.dm.)
cor(data$X.bgr.,data$X.dm.)

#Packed Cell Volume vs Chronic Kidney Disease Box plot
boxplot(data$X.pcv.[which(data$X.class.==0)],data$X.pcv.[which(data$X.class.==1)],
        main="Packed Cell Volume vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Packed Cell Volume',names=c("No","Yes"))


#Sodium vs Chronic Kidney Disease Box plot
boxplot(data$X.sod.[which(data$X.class.==0)],data$X.sod.[which(data$X.class.==1)],
        main="Sodium vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Sodium',names=c("No","Yes"))

#Blood Urea vs Chronic Kidney Disease Box plot
boxplot(data$X.bu.[which(data$X.class.==0)],data$X.bu.[which(data$X.class.==1)],
        main="Blood Urea vs Chronic Kidney Disease", xlab='Chronic Kidney Disease', 
        ylab='Blood Urea',names=c("No","Yes"))
