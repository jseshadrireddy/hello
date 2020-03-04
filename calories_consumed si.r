install.packages("reader")
library(readr)
Calories_consumed= read.csv(file.choose())
View(Calories_consumed)
#Explonatory data analysis
summary(Calories_consumed)
#scotter plot
plot(Calories_consumed$Weight.gained..grams.)
plot(Calories_consumed$Calories.Consumed)
?plot
attach(Calories_consumed)
Weight.gained..grams.
Calories.Consumed
#corilation coefficient(r)
cor(Weight.gained..grams.,Calories.Consumed)
#simple linear regression model
reg=lm(Weight.gained..grams.~Calories.Consumed)
?lm
summary(reg)
pred=predict(reg)
pred
reg$residuals
plot(reg$residuals)
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(Calories_consumed))
sqrt(mean(reg$residuals^2))
confint(reg,level = 0.95)
predict(reg,interval = "predict")
predict
library(ggplot2)
?ggplot2
ggplot(data = Calories_consumed,aes(x=Weight.gained..grams.,y=Calories.Consumed))+
  geom_point(color='blue')+
  geom_line(color='red',data = Calories_consumed,aes(x=Weight.gained..grams.,y=pred))
######################
#logerithamic model
#x=log(waist);y=AT
plot(log(Weight.gained..grams.),Calories.Consumed)
cor(log(Weight.gained..grams.),Calories.Consumed)
reg_log=lm(Calories.Consumed~log(Weight.gained..grams.))
summary(reg_log)
predict(reg_log)
reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(Calories_consumed))
confint(reg_log,level = 0.95)
predict(reg_log,interval = "confidence")
#Exponential model
#x=waist and y=log(AT)
plot(Weight.gained..grams.,log(Calories.Consumed))
cor(Weight.gained..grams.,log(Calories.Consumed))
reg_exp=lm(log(Calories.Consumed)~Weight.gained..grams.)#lm(log(y)~x)
summary(reg_exp)
reg_exp$residuals
sqrt(mean(reg_exp$residuals))
logat=predict(reg_exp)
logat
at=exp(logat)
error=Calories_consumed$Calories.Consumed-at
error
sqrt(sum(error^2)/nrow(Calories_consumed))
confint(reg_exp,level = 0.95)
predict(reg_exp,interval = "confidence")
#####################
#polynomial model with 2 degree(quadratic model)
plot(Weight.gained..grams.,Calories.Consumed)
plot(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed)
cor(Weight.gained..grams.*Weight.gained..grams.,Calories.Consumed)
plot(Weight.gained..grams.*Weight.gained..grams.,log(Calories.Consumed))
cor(Weight.gained..grams.,log(Calories.Consumed))
cor(Weight.gained..grams.*Weight.gained..grams.,log(Calories.Consumed))
#lm(y~x+Ix*x+....+I(x*x*x...))
reg2degree=lm(log(Calories.Consumed)~Weight.gained..grams.+I(Weight.gained..grams.+Weight.gained..grams.))
summary(reg2degree)
logpol=predict(reg2degree)
expy=exp(logpol)
expy
err=Calories_consumed$Calories.Consumed-expy
err
sqrt(sum(err^2)/nrow(Calories_consumed))
confint(reg2degree,level = 0.95)
predict(reg2degree,interval = "confidence")
#visuvalization
ggplot(data = Calories_consumed,aes(x=Weight.gained..grams.+I(Weight.gained..grams.^2),y=log(Calories.Consumed)))+
  geom_point(color='blue')+
  geom_line(color='red',data = Calories_consumed,aes(x=Weight.gained..grams.+I(Weight.gained..grams.^2),y=logpol))
#####
#polynomial model 3 degree
reg3degree=lm(log(Calories.Consumed)~Weight.gained..grams.+I(Weight.gained..grams.+Weight.gained..grams.)+I(Weight.gained..grams.+Weight.gained..grams.+Weight.gained..grams.))
summary(reg3degree)
logpol3=predict(reg3degree)
expy3=exp(logpol3)
expy3
#visuallization
ggplot(data = Calories_consumed,aes(x=Weight.gained..grams.+I(Weight.gained..grams.^2)+I(Weight.gained..grams.^3),y=Calories.Consumed))+
  geom_point(color='blue')+
  geom_line(color='red',data = Calories_consumed,aes(x=Weight.gained..grams.+I(Weight.gained..grams.^2)+I(Weight.gained..grams.^3),y=logpol3))

