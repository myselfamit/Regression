View(faithful)
attach(faithful)     # attach the data frame 
names(faithful)
str(faithful)
head(faithful,10)
# behaviour and missing value and univariate analysis
summary(faithful)
## another way missing value--------
sapply(faithful, function(x) sum(is.na(x)))
### scatter plot and Correlation
plot(waiting,eruptions) 
plot(eruptions~waiting) 
cor(faithful)
input<-faithful
### Regression Model
eruption.lm = lm(eruptions ~ waiting , data= input)
summary(eruption.lm)
anova(eruption.lm)
### Assumption of regression Model
## below command will plot all the assumpution plot in 2*2 format 
par(mfrow=c(2,2))
plot(eruption.lm)

library(lmtest)
dwtest(eruption.lm)
### linearity
plot(eruptions~waiting) 
# Prediction of 80 waiting
y=-1.87402+0.07563*80
y

## JUST TO CHECK MATHEMATICALLY of linear Model
input<-faithful
Probability<-data.frame(eruption.lm$fitted.values)
Residual<-data.frame(eruption.lm$residuals)
input$Fitted_value<-Probability$eruption.lm.fitted.values
input$Residual<-Residual$eruption.lm.residuals

plot(eruptions~waiting) 
abline(eruption.lm,col="red")

#Predicton Method on entire testing data set  
newdata = data.frame(waiting=80)
predict(eruption.lm, newdata) 
 



 lor 

coeffs = coefficients(eruption.lm); coeffs 
waiting=80
duration = coeffs[1] + coeffs[2]*waiting 
duration

or 

y=-1.87402+0.07563*80
y


#####task
model<-read.csv(file.choose())
attach(model)
plot(INCOME,SAVINGS)
cor(model)
linearmodel = lm(SAVINGS ~ INCOME , data = model)
linearmodel
summary(linearmodel)
par(mfrow=c(2,2))
plot(linearmodel)
# predict in mathematical way 
y=-10991+0.2970*256413
y


