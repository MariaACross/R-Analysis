## 1. Installing and loading the Packages 
install.packages("corrplot")
install.packages("gtsummary")
install.packages("dummies")
install.packages("fastDummies")
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")
install.packages("ggiraphExtra")
library(ggiraphExtra)
require(ggiraph)
require(ggiraphExtra)
library(devtools)
library(dplyr)
library(corrplot)
library(gtsummary)
library(fastDummies)
library(dummies)
library(ggplot2)

## 2. loading the dataset
lung = read.csv("LungTumorStudy.csv")

## 3. understanding the dataset
head(lung)
summary(lung)
View(lung)

#Age vs Smoke
ggplot(lung, aes(x= Age, fill=Smoke)) + 
  geom_bar(position = 'dodge') +
  xlab("Age") +
  ylab("Tally") +
  ggtitle("Age vs Smoke Status") +
  scale_fill_discrete(name = "Smokers", labels = c("No", "Yes"))

#Age vs Gender
ggplot(lung, aes(x= lung$Age, fill=lung$Gender)) + 
  geom_bar(position = 'dodge') +
  xlab("Age") +
  ylab("Count") +
  ggtitle("Analysis of Age by Gender") +
  scale_fill_discrete(name = "Gender")


## 4. Part 1- Regression analysis for creating dummy variables

#Creating subsets
Male =lung[lung$Gender=="male",]
Female <- lung[lung$Gender=="female",]
Smoker <- lung[lung$Smoke=="yes",]
Non_Smoker <- lung[lung$Smoke=="no",]

plot(lung$TumorSize~lung$Days)
lm(lung$TumorSize~lung$Days)

require(ggplot2)
ggplot(lung,aes(y=TumorSize,x=Days))+geom_point()+geom_smooth(method="lm")

plot(lung$TumorSize~lung$Age)
model1<-lm(lung$TumorSize~lung$Age)
require(ggplot2)
ggplot(lung,aes(y=TumorSize,x=Age))+geom_point()+geom_smooth(method="lm")

model2<-lm(TumorSize~Age+Gender, data = lung)
eq1=function(x){coef(model2)[2]*x+coef(model2)[1]}
eq2=function(x){coef(model2)[2]*x+coef(model2)[1]+coef(model2)[3]}

ggplot(data = lung ,aes(y=TumorSize,x=Age,color=Gender))+geom_point()+
  stat_function(fun=eq1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=eq2,geom="line",color=scales::hue_pal()(2)[2])

ggPredict(model2,se=TRUE,interactive=TRUE)

model3<-lm(TumorSize~Age*Asbestos, data = lung)
eq1=function(x){coef(model3)[2]*x+coef(model3)[1]}
eq2=function(x){coef(model3)[2]*x+coef(model3)[1]+coef(model3)[3]}

ggplot(data = lung ,aes(y=TumorSize,x=Age,color=Age))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

ggPredict(model3,se=TRUE,interactive=TRUE)

#Creating Dummy Variables
df1<-dummy(data$Smoke, sep = "_")
lung<-cbind(lung, df1)
df2<-dummy(data$Gender, sep = "_")
lung<-cbind(lung, df2)
df3<-dummy(data$Asbestos, sep = "_")
lung<-cbind(lung, df3)
df4<-dummy(data$TreatmentA, sep = "_")
lung<-cbind(lung, df4)

lung[c("Smoke", "Gender", "Asbestos", "TreatmentA")]<-NULL
head(lung)

summary(lung)


model4<-lm(TumorSize ~Gender_female + Smoke_yes, data = lung)
model4

require(ggplot2)
ggplot(lung,aes(y=TumorSize,x=Asbestos_yes + Smoke_yes))+geom_point()+geom_smooth(method="lm")

## 5. Part 2 - Regression for each subset
#Regression for each subset
#Male
mod_male <- lm(TumorSize ~ Days, data = Male)
summary(mod_male)
ggplot(Male, aes(x = Days, y = TumorSize )) +  geom_point( color="#94ce7b", size=6, alpha = 0.5) +  
  ggtitle("Tumor Size for Male") +  xlab("Days") +  ylab("Tumor Size")+ stat_smooth(method = lm, se = F)

#Smoker
mod_Smoker <- lm(TumorSize ~ Days, data = Smoker)
summary(mod_Smoker)
ggplot(Smoker, aes(x = Days, y = TumorSize )) +  geom_point( color="#f3d113", size=6, alpha = 0.5) +  
  ggtitle("Tumor Size for Smokers") +  xlab("Days") +  ylab("Tumor Size")+ stat_smooth(method = lm, se = F)
ggpred 
