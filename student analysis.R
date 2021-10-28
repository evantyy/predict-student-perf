setwd("D:/et4_e")

# Import and prepare the student performance dataset
school1=read.table("student-mat.csv",sep=";",header=TRUE)
school2=read.table("student-por.csv",sep=";",header=TRUE)
schools=merge(school1,school2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))


# Changing all characters to factors
school1[sapply(school1, is.character)] <- lapply(school1[sapply(school1, is.character)], 
                                                 as.factor)
school2[sapply(school2, is.character)] <- lapply(school2[sapply(school2, is.character)], 
                                                 as.factor)
schools[sapply(schools, is.character)] <- lapply(schools[sapply(schools, is.character)], 
                                                 as.factor)

sum(is.na(school1))
sum(is.na(school2))
sum(is.na(schools))


# Brief details of data given---------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

table(school1$school)
table(school2$school)
table(schools$school)

table1 <- rbind(c(mean(school1$G1), mean(school1$G2), mean(school1$G3)),c(mean(schools$G1.x), mean(schools$G2.x), mean(schools$G3.x)))
colnames(table1) <-c("G1","G2","G3")
rownames(table1) <- c("Math","Math&Port")
table1
## Mean Grade of students in math have improved throughout period.

table2 <- rbind(c(mean(school2$G1), mean(school2$G2), mean(school2$G3)),c(mean(schools$G1.y), mean(schools$G2.y), mean(schools$G3.y)))
colnames(table2) <-c("G1","G2","G3")
rownames(table2) <- c("Port","Port&Math")
table2
## Mean Grade of students in Portuguese have improved throughout period.


# Finding relationship between variables and Final Grade-----------------------------
ggplot(schools, aes(x=age))+geom_density()
## No normal distribution so age varies

# Reason affecting grades
ggplot(school1, aes(x=G3, fill = reason))+ geom_density(alpha = .3)+ggtitle("Reason affecting Math grade")
ggplot(school2, aes(x=G3, fill = reason))+ geom_density(alpha = .3)+ggtitle("Reason affecting Portuguese grade")

# Address affecting grades
ggplot(school1, aes(x=G3, fill = address))+ geom_density(alpha = .3)+ggtitle("Address affecting Math grade")
ggplot(school2, aes(x=G3, fill = address))+ geom_density(alpha = .3)+ggtitle("Address affecting Portuguese grade")
## Students in Urban region have higher mean final grade than rural region.

# School affecting grades
ggplot(school1, aes(x= G3, fill = school))+ geom_density(alpha = .3)+ggtitle("School affecting Math grade")
ggplot(school2, aes(x= G3, fill = school))+ geom_density(alpha = .3)+ggtitle("School affecting Portuguese grade")

# Desire for higher education affecting grades
ggplot(school1, aes(x= G3, fill = higher))+ geom_density(alpha = .3)+ggtitle("Higher Edu desire affecting Math grade")
ggplot(school2, aes(x= G3, fill = higher))+ geom_density(alpha = .3)+ggtitle("Higher Edu desire affecting Portuguese grade")

# Father's Education affecting grades
ggplot(school1, aes(x= G3))+ geom_boxplot(aes(color=as.factor(Fedu)))+ggtitle("Father's Education affecting Math grade")
ggplot(school2, aes(x= G3))+ geom_boxplot(aes(color=as.factor(Fedu)))+ggtitle("Father's Education affecting Portuguese grade")

# Mother's Education affecting grades
ggplot(school1, aes(x= G3))+ geom_boxplot(aes(color=as.factor(Medu)))+ggtitle("Mother's Education affecting Math grade")
ggplot(school2, aes(x= G3))+ geom_boxplot(aes(color=as.factor(Medu)))+ggtitle("Mother's Education affecting Portuguese grade")


# Hold-out Validation method----------------------------------------------------
set.seed(123)
library(caTools)

Msample = sample.split(school1, SplitRatio = 0.7)
Mtrain = subset(school1, Msample==TRUE,select = -c(G1,G2))
Mtest = subset(school1, Msample==FALSE,select = -c(G1,G2))

Psample = sample.split(school2, SplitRatio = 0.7)
Ptrain = subset(school2, Psample==TRUE,select = -c(G1,G2))
Ptest = subset(school2, Psample==FALSE,select = -c(G1,G2))

# Linear Regression equations without G1, G2 variables-----------------------------------------
m1 <- lm(scale(G3) ~ ., data=Mtrain)
summary(m1)
## failures, goout, internet, romantic relationship, gender are the significant variables for Math course

m2 <- lm(scale(G3) ~ ., data=Ptrain)
summary(m2)
## school, higher edu, failures, school support are significant variables of Portuguese language course


# Relationship of failures with G3 for Math and G3 in Portuguese language-------
m1failure <- lm(G3 ~failures, data = Mtrain)
summary(m1failure)

plot(x = school1$failures, y = school1$G3,
     pch = 18,
     col = gray(.1, .1),
     xlab = "Past Class Failures",
     ylab = "Final Grade",
     main = "Math Students"
)
abline(m1failure, lty = 2)
## Final Grade scores have a negative linear relationship with the amount of past class failures for Math.

m2failure <- lm(G3 ~failures, data = Ptrain)
plot(x = school2$failures, y = school2$G3,
     pch = 20,
     col = gray(.1, .1),
     xlab = "Past Class Failures",
     ylab = "Final Grade",
     main = "Portuguese language Students"
)
abline(m2failure, lty = 2)
## Final grade scores have a negative linear relationship with the amount of past class failures for Portuguese language.


# Predicting data using Linear Regression

#Math prediction
sch1_m1 <- lm(G3 ~ sex + age + goout + failures,data=Mtrain)
summary(sch1_m1)

# Predicting model with selective dataframe
newdf1 <- data.frame(sex = "F", age = 18, goout = 5,failures = 3)
predict(object = sch1_m1,newdata = newdf1)
## An 18 year old female student who went out 5 times and have 3 past class failures will be predicted to have a final math grade of 3.398869.


# Portuguese language prediction
sch2_m1 <- lm(G3 ~ sex+ age+ higher+ failures+ schoolsup,data=Ptrain)
summary(sch2_m1)

# Predicting the model with selective dataframe
newdf2 <- data.frame(sex = "M", age = 19, higher = "yes", failures = 1, schoolsup = "yes")
predict(object = sch2_m1, newdata = newdf2 )
## A 19 year old Male student who wants to pursue higher education, have 2 class failures and school support will be predicted to have a final Portuguese language grade of 9.910286.


#Checking accuracy with RMSE
library(caret)

predictions1 <- m1 %>% predict(Mtest)
RMSE(predictions1, Mtest$G3)
predictions1 <- m1 %>% predict(Mtrain)
RMSE(predictions1, Mtrain$G3)


predictions3 <- m2 %>% predict(Ptest)
RMSE(predictions1, Ptest$G3)
predictions3 <- m2 %>% predict(Ptrain)
RMSE(predictions1, Ptrain$G3)
# The RMSE for train and test sets are very similar, hence a good model.

# Plotted data ---------------------------------------------
plot(m1)
plot(m2)

# Random Forest-----------------------------------------------------------------
library(randomForest)

# Cleaning and creating new dataframe
newdata1.rf = school1
newdata1.rf$passed = factor(ifelse(school1$G3 >= 10, "yes", "no"))
newdata2.rf = school2
newdata2.rf$passed = factor(ifelse(school2$G3 >= 10, "yes", "no"))
newdata1.rf = subset(newdata1.rf, select = -c(G1,G2,G3))
newdata2.rf = subset(newdata2.rf, select = -c(G1,G2,G3))

# making RF with 100 trees
pre <- ncol(newdata1.rf)
passedmath_rf <- randomForest(passed~., data=newdata1.rf, mtry=pre, ntree=100)
passedportuguese_rf <- randomForest(passed~., data=newdata2.rf, mtry=pre, ntree=100)

importance(passedmath_rf)
varImpPlot(passedmath_rf)

importance(passedportuguese_rf)
varImpPlot(passedportuguese_rf)
# Both absences and amount of past failed classes are influential variables.

passedmath_rf
passedportuguese_rf
## About 70% accuracy for Math, and 84% accuracy for Portuguese

# Ridge Regression--------------------------------------------------------------
library(glmnet)
library(caTools)
Mridge <- select_if(school1,is.numeric)
Pridge <- select_if(school2,is.numeric)

# Train & Testsets for Math students
Msample <- sample.split(Mridge, SplitRatio = 0.7)
Mtrain.RR <- na.omit(Mridge[Msample,])
Mtrain.X <- as.matrix(Mtrain.RR[,-c(14,15,16)])
Mtrain.Y <- Mtrain.RR[,16]

Mtest.RR <- na.omit(Mridge[-Msample,])
Mtest.X <- as.matrix(Mtest.RR[,-c(14,15,16)])
Mtest.Y <- Mtest.RR[,16]

# Train & Testsets for Portuguese students
Psample <- sample.split(Pridge, SplitRatio = 0.7)
Ptrain.RR <- na.omit(Pridge[Psample,])
Ptrain.X <- as.matrix(Ptrain.RR[,-c(14,15,16)])
Ptrain.Y <- Ptrain.RR[,16]

Ptest.RR <- na.omit(Pridge[-Psample,])
Ptest.X <- as.matrix(Ptest.RR[,-c(14,15,16)])
Ptest.Y <- Ptest.RR[,16]

schgrid <- 10^seq(-3,1,length=100)

# Math Students
M_ridge.m1 <- glmnet(Mtrain.X, Mtrain.Y, alpha = 1,intercept = FALSE, lambda = schgrid)

Mcv <- cv.glmnet(Mtrain.X, Mtrain.Y, alpha = 1,intercept = FALSE, lambda = schgrid)
plot(Mcv)
goodlamda <- Mcv$lambda.min
goodlamda
M_ridge.pd <- predict(M_ridge.m1, s = goodlamda, newx = Mtest.X)
M_RMSE.ridge <- sqrt(mean((M_ridge.pd-Mtest.Y)^2))
M_RMSE.ridge

M_glm <- glmnet(Mtrain.X, Mtrain.Y,alpha=1,lambda = schgrid)
M_ridge.coef <- predict(M_glm,type="coefficients",s=goodlamda)
M_ridge.coef

# Portuguese
P_ridge.m1 <- glmnet(Ptrain.X, Ptrain.Y, alpha = 1,intercept = F, lambda = schgrid)

Pcv <- cv.glmnet(Ptrain.X, Ptrain.Y, alpha = 1,intercept = F, lambda = schgrid)
plot(Pcv)
goodlamda2 <- Pcv$lambda.min
goodlamda2
P_ridge.pred <- predict(P_ridge.m1, s = goodlamda2, newx = Ptest.X)
P_RMSE.ridge <- sqrt(mean((P_ridge.pred-Ptest.Y)^2))
P_RMSE.ridge

P_out <- glmnet(Ptrain.X, Ptrain.Y,alpha=1,lambda=schgrid)
P_ridge.coef <- predict(P_out,type="coefficients",s=goodlamda2)
P_ridge.coef

