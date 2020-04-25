startup <- read.csv(file.choose())
View(startup)
toyoto=startup[,-c(1,2,5,6,8,10,11,12,15,19,20,21,22,23,24,25,26,27,28,29,30,
                   31,32,33,34,35,36,37,38)]
View(toyoto)
summary(toyoto)
pairs(toyoto)
cor(toyoto)
sum(is.na(toyoto))
boxplot(toyoto)
####visuvalization
ggplot(data=toyoto,aes(x =toyoto$HP, fill = toyoto$Price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='toyoto dataset on hp variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
###
ggplot(data=toyoto,aes(x =toyoto$Price, fill = toyoto$Price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='toyoto dataset on price variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=toyoto,aes(x =toyoto$Age_08_04, fill = toyoto$Price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='toyoto dataset onAge_08_04 variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=toyoto,aes(x =toyoto$KM, fill = toyoto$Price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='toyoto dataset on km variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
##
ggplot(data=toyoto,aes(x =toyoto$cc, fill = toyoto$Price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='toyoto dataset on cc variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=toyoto,aes(x =toyoto$Doors, fill = toyoto$Price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='toyoto dataset on doors  variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=toyoto,aes(x =toyoto$Gears, fill = toyoto$Price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='toyoto dataset on gears variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=toyoto,aes(x =toyoto$Quarterly_Tax, fill = toyoto$Price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='toyoto dataset on quarterly_tax variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
##
ggplot(data=toyoto,aes(x =toyoto$Weight, fill = toyoto$Price)) +
  geom_histogram()+
  theme(panel.background = element_rect(fill = 'peachpuff'))+
  labs(title ='toyoto dataset on weight variable')+
  theme(plot.title = element_text(hjust = 0.5),plot.background = 
          element_rect('aquamarine4'))
###
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(toyoto))
model_ty=lm(Price~.,data = toyoto)
summary(model_ty)
model_ty1=lm(Price~Age_08_04,data = toyoto)
summary(model_ty1)
model_ty2=lm(Price~KM,data = toyoto)
summary(model_ty2)
model_ty3=lm(Price~HP,data = toyoto)
summary(model_ty3)
model_ty4=lm(Price~cc,data = toyoto)
summary(model_ty4)
model_ty5=lm(Price~Doors,data = toyoto)
summary(model_ty5)
model_ty6=lm(Price~Gears,data = toyoto)
summary(model_ty6)
model_ty7=lm(Price~Quarterly_Tax,data = toyoto)
summary(model_ty7)
model_ty8=lm(Price~Weight,data = toyoto)
summary(model_ty8)
model_tyt=lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = toyoto)
summary(model_tyt)
influence.measures(model_ty)
library(car)
vif(model_ty)
avPlots(model_ty,id.n=2,id.cex=0.7)
library(psych)
influenceIndexPlot(model_ty,id.n=3)
influencePlot(model_ty,id.n=5)
model_10=lm(Price~.-Doors,data = toyoto[-81,])
summary(model_10)
model_12=lm(Price~.-Doors,data = toyoto[-c(81,961),])
summary(model_12)
###final model##
plot(lm(Price~.-Doors,data = toyoto[-c(81)]))
summary(lm(Price~.-Doors,data = toyoto[-c(81)]))
plot(lm(Price~.-Doors,data = toyoto[-c(81,961)]))
summary(lm(Price~.-Doors,data = toyoto[-c(81,961)]))
plot(lm(Price~.-Doors,data = toyoto[-c(81,222,961)]))
summary(lm(Price~.-doors,data = toyoto[-c(81,222,961)]))
finalmodel=(lm(Price~.-Doors,data = toyoto[-c(222,961)]))
summary(finalmodel)
plot(finalmodel)
hist(residuals(finalmodel))
