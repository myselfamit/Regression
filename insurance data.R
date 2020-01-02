# insurance data imported
data<-read.csv("E:/R files/Linear regression/insurance.csv")

# Removing the irrelavent variable
data$region<-NULL

# Checking the Data type
names(data)
str(data)

# count of levels in a variable 
table(data$smoker)
table(data$sex)

# converting categorical data to numeric 
data$Smoker_flag <- ifelse(data$smoker=='yes',1,2)
data$sex_Flag<-ifelse(data$sex=='male',1,2)

# Data Conversion
str(data)
data$age<-as.numeric(data$age)
data$children<-as.numeric(data$children)
data$Smoker_flag <-as.numeric(data$Smoker_flag)
data$sex_Flag<-as.numeric(data$sex_Flag)
data$Month<-as.numeric(data$Month)

# Checking the Data type
str(data)

# To identify  outlier
boxplot(data$age)
boxplot(data$bmi)
boxplot(data$children)
boxplot(data$charges)
boxplot(data$Month)

# Treatment of outlier for bmi
summary(data$bmi)
upper<-34.69+1.5*IQR(data$bmi);upper
data$bmi[data$bmi > upper]<-upper
boxplot(data$bmi)
summary(data$bmi)

# Treatment of outlier for charges
summary(data$charges)
upper<-16640+1.5*IQR(data$charges);upper
data$charges[data$charges > upper]<-upper
boxplot(data$charges)
summary(data$charges)

# data Subset
abc<-data[,-c(2,5)]     

#data Partition# 546  # 123
set.seed(789)
library(caret)
Train <- createDataPartition(abc$charges, p=0.70,list=FALSE)
training <- abc[ Train, ]
testing <- abc[ -Train, ]

# model building and may leads to overfitting
cor(training)
model<-lm(charges~.,data = training)
summary(model)

# identifying the solution to overfitting 
# with the help of transformation
hist(training$charges)
hist((1/training$charges))
hist(log(training$charges))

# model 2
model2<-step(lm(log(charges)~.,data = training)
             ,direction = "backward")
summary(model2)

#collinearity or multi-collinearity
library(car)
vif(model2)

#Assumption of linear Regression
par(mfrow=c(2,2))
plot(model2)

# model error assumption
library(lmtest)
dwtest(model2)

# constant variance assumption in term of Numerica method
ncvTest(model2)

#Prediction
testing$fitted<-predict(model2,testing)

# transforming  log values to real value
testing$Original<-exp(testing$fitted)





