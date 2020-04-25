startup <- read.csv(file.choose())
st50=startup[,-4]
View(st50)
View(startup)
summary(st50)
summary(startup)
pairs(st50)
pairs(startup)
cor(st50)

##visuvalization on histograms with st50 data set
ggplot(data=st50,aes(x =st50$R.D.Spend, fill = st50$Profit)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='st50 dataset on R.D.Spend variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=st50,aes(x =st50$Administration, fill = st50$Profit)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='st50 dataset on Administrations variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=st50,aes(x =st50$Marketing.Spend, fill = st50$Profit)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='st50 dataset on Marketing.spend variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=st50,aes(x =st50$Profit, fill = st50$Profit)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='st50 dataset on profit variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
###
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(st50))
model_50=lm(Profit~.,data=st50)
summary(model_50)
model_50A=lm(Profit~R.D.Spend,data=st50)
summary(model_50A)
model_50S=lm(Profit~Administration,data=st50)
summary(model_50S)
model_50R=lm(Profit~Marketing.Spend,data=st50)
summary(model_50R)
model_50SR=lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=st50)
summary(model_50SR)
influence.measures(model_50)
library(car)
vif(model_50)
avPlots(model_50,id.n=2,id.cex=0.7)
library(psych)
influenceIndexPlot(model_50,id.n=3)
influencePlot(model_50,id.n=5)
model_10=lm(Profit~.-Administration,data = st50[-50,])
summary(model_10)
model_12=lm(Profit~.-Administration,data = st50[-c(49,50),])
summary(model_12)
###final model##
plot(lm(Profit~.-Administration,data = st50[-c(50)]))
summary(lm(Profit~.-Administration,data = st50[-c(50)]))
plot(lm(Profit~.-Administration,data = st50[-c(49,50)]))
summary(lm(Profit~.-Administration,data = st50[-c(49,50)]))
plot(lm(Profit~.-Administration,data = st50[-c(47,49,50)]))
summary(lm(Profit~.-Administration,data = st50[-c(47,49,50)]))
finalmodel=(lm(Profit~.-Administration,data = st50[-c(49,50)]))
summary(finalmodel)
plot(finalmodel)
hist(residuals(finalmodel))
#####################
#polynomial model with 2 degree(quadratic model)
plot(Profit,Administration)
attach(st50)
plot(Profit*Profit,Administration)
cor(Profit*Profit,Administration)
plot(Profit*Profit,log(Administration))
cor(Profit,log(Administration))
cor(Profit*Profit,log(Administration))
#lm(y~x+Ix*x+....+I(x*x*x...))
reg2degree=lm(log(Administration)~Profit+I(Profit+Profit))
summary(reg2degree)
logpol=predict(reg2degree)
expy=exp(logpol)
expy
err=st50$Administration-expy
err
sqrt(sum(err^2)/nrow(st50))
confint(reg2degree,level = 0.95)
predict(reg2degree,interval = "confidence")
#visuvalization
install.packages('ggplot2')
library(ggplot2)
ggplot(data = st50,aes(x=Profit+I(Profit^2),y=log(Administration)))+
  geom_point(color='blue')+
  geom_line(color='red',data = st50,aes(x=Profit+I(Profit^2),y=logpol))
#####
#polynomial model 3 degree
reg3degree=lm(log(Administration)~Profit+I(Profit+Profit)+I(Profit+Profit+Profit))
summary(reg3degree)
logpol3=predict(reg3degree)
expy3=exp(logpol3)
expy3
#visuallization
ggplot(data = st50,aes(x=Profit+I(Profit^2)+I(Profit^3),y=Administration))+
  geom_point(color='blue')+
  geom_line(color='red',data = st50,aes(x=Profit+I(Profit^2)+I(Profit^3),y=logpol3))
