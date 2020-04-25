computer <- read.csv(file.choose())
com=computer[,-c(7,8,9)]
View(com)
summary(com)
pairs(com)
cor(com)
sum(is.na(com))
#####
library(ggplot2)
boxplot(com)
plot(com)
###histograms
ggplot(data=com,aes(x =com$speed, fill = com$price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='computer dataset on speed')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=com,aes(x =com$hd, fill = com$price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='computer dataset on hd')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=com,aes(x =com$ram, fill = com$price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='computer dataset on ram')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=com,aes(x =com$screen, fill = com$price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='computer dataset on screen')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=com,aes(x =com$ads, fill = com$price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='computer dataset on ads')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
###
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(com))
model_50=lm(price~.,data = com)
summary(model_50)
model_50a=lm(price~speed,data = com)
summary(model_50a)
model_50s=lm(price~hd,data = com)
summary(model_50s)
model_50j=lm(price~ram,data = com)
summary(model_50j)
model_50e=lm(price~screen,data = com)
summary(model_50e)
model_50f=lm(price~ads,data = com)
summary(model_50f)
model_50w=lm(price~speed+hd+ram+screen+ads,data = com)
summary(model_50w)
influence.measures(model_50)
library(car)
vif(model_50)
avPlots(model_50,id.n=2,id.cex=0.7)
library(psych)
influenceIndexPlot(model_50,id.n=3)
influencePlot(model_50,id.n=5)
model_10=lm(price~.-hd,data = com[-4478,])
summary(model_10)
model_12=lm(price~.-hd,data = com[-c(1701,4478),])
summary(model_12)
###final model##
plot(lm(price~.-hd,data = com[-c(4478)]))
summary(lm(price~.-hd,data = com[-c(4478)]))
plot(lm(price~.-hd,data = com[-c(1701,4478)]))
summary(lm(price~.-hd,data = com[-c(1701,4478)]))
plot(lm(price~.-hd,data = com[-c(1701,3784,4478)]))
summary(lm(price~.-hd,data = com[-c(1701,3784,4478)]))
finalmodel=(lm(price~.-hd,data = com[-c(1701,4478)]))
summary(finalmodel)
plot(finalmodel)
hist(residuals(finalmodel))
