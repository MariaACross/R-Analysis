#import the dataset
heart = read.csv("Heart.csv")

#understand the data
summary(heart)
str(heart)

#install and use corrplot package to analyse the correlation between variables
install.packages("corrplot")
library(corrplot)

corrplot(cor(heart), type="upper")

#delete irrelevant data
heart = subset(heart, select=c(-restecg, -chol,-fbs))

#change necessary categorical values to string values 
#that are easily understandable
heart$sex[heart$sex == 0] = "female"
heart$sex[heart$sex == 1] = "male"
heart$cp[heart$cp == 0] = "typical angina"
heart$cp[heart$cp == 1] = "atypical angina"
heart$cp[heart$cp == 2] = "non-anginal pain"
heart$cp[heart$cp == 3] = "asymptomatic"
heart$exang[heart$exang == 0] = "no"
heart$exang[heart$exang == 1] = "yes"
heart$slope[heart$slope == 0] = "upsloping"
heart$slope[heart$slope == 1] = "flat"
heart$slope[heart$slope == 2] = "downsloping"
heart$thal[heart$thal == 1] = "normal"
heart$thal[heart$thal == 2] = "fixed defect"
heart$thal[heart$thal == 3] = "reversible defect"
heart$target1 = heart$target
heart$target1[heart$target1 == 0] = "no heart disease"
heart$target1[heart$target1 == 1] = "heart disease"

#Data is ready for Analysis

#check the summary of dataset
str(heart)
summary(heart)

#checking how many people have a heart disease
table(heart['target1'])

#checking how many males and females are there in the dataset
table(heart['sex'])

#analyzing correlation between sex and heart condition
round(table(heart$sex,heart$target1),2)
round(prop.table(table(heart$sex, heart$target1)), 2)

#installing ggplot2 package to plot graphs
install.packages("ggplot2")
library(ggplot2)

#Plot to check distribution of the age of the Population
qplot(heart$age,geom = 'histogram', main = 'Age of Population', 
      xlab = 'Age', ylab = 'Frequency',binwidth=0.5)

#Plot to analyze Variation of age with Heart Condition
ggplot(heart, aes(x= age, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Age") +
  ylab("Tally") +
  ggtitle("Age vs Heart Condition Analysis") +
  scale_fill_discrete(name = "Heart Failures", labels = c("Yes", "No"))

#Computing two new attributes Male and Female data
male_heart = heart[heart$sex=="male",]
female_heart = heart[heart$sex=="female",]

#Analyzing heart condition of Males across different ages
ggplot(male_heart, aes(x= male_heart$age, fill=male_heart$target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Age") +
  ylab("Tally") +
  ggtitle("Age vs Heart Condition Analysis of Males") +
  scale_fill_discrete(name = "Heart Failures", labels = c("Yes", "No"))

#Analyzing heart condition of Females across different ages
ggplot(female_heart, aes(x= female_heart$age, fill=female_heart$target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Age") +
  ylab("Tally") +
  ggtitle("Age vs Heart Condition Analysis of Females") +
  scale_fill_discrete(name = "Heart Failures", labels = c("Yes", "No"))

#Plot to Analyse Slope type
ggplot(heart, aes(x= slope, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Slope Types") +
  ylab("Tally") +
  ggtitle("Slope Types Analysis") +
  scale_fill_discrete(name = "Heart Failures", labels = c("Yes", "No"))

#Plot to analyze the Heart Condition by ST depression Induced by Exercise relative to rest 
ggplot(heart, aes(x = target1, y = oldpeak)) +
  geom_boxplot() + xlab("Heart Condition") + 
  ylab("ST Depression") + 
  ggtitle("ST Depression vs Heart Condition")

#Plot to analyse correlation between the number of heart vessels and Heart Condition
ggplot(heart, aes(x= ca, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("#of Heart Vessels") +
  ylab("Count") +
  ggtitle("Analysis of Heart Diseases by # of Heart Vessels") +
  scale_fill_discrete(name = "Heart Failures", labels = c("Yes", "No"))

#Plot to analyse correlation between the number of heart vessels in Males and Heart Condition
ggplot(male_heart, aes(x= male_heart$ca, fill=male_heart$target1)) + 
  geom_bar(position = 'dodge') +
  xlab("#of Heart Vessels") +
  ylab("Count") +
  ggtitle("Analysis of Heart Diseases by # of Heart Vessels in Males") +
  scale_fill_discrete(name = "Heart Failures", labels = c("Yes", "No"))

#Plot to analyse correlation between the number of heart vessels in Females and Heart Condition
ggplot(female_heart, aes(x= female_heart$ca, fill=female_heart$target1)) + 
  geom_bar(position = 'dodge') +
  xlab("#of Heart Vessels") +
  ylab("Count") +
  ggtitle("Analysis of Heart Diseases by # of Heart Vessels in Females") +
  scale_fill_discrete(name = "Heart Failures", labels = c("Yes", "No"))

#Plot to Analyse correlation between Heart Condition and chest pain type
mosaicplot(table(heart$target1, heart$cp), 
           col=c("#4682B4", "#00468B", "#00008B", "#46008B"), 
           las=1, main="Heart Disease for Chest Pain Type")

#Plot to Analyse correlation between Heart Condition and Thalassemia
mosaicplot(table(heart$target1, heart$thal), 
           col=c( "#4682B4", "#B4464B", "#B4AF46"), 
           las=1, main="Heart Disease for Thalassemia")

#Plot to Analyse correlation between Heart Condition and Thalassemia in Males
mosaicplot(table(male_heart$target1, male_heart$thal), 
           col=c("#4682B4", "#B4464B", "#B4AF46"), 
           las=1, main="Heart Disease in Males for Thalassemia")

#Plot to Analyse correlation between Heart Condition and Thalassemia in Females
mosaicplot(table(female_heart$target1, female_heart$thal), 
           col=c( "#4682B4", "#B4464B", "#B4AF46"), 
           las=1, main="Heart Disease in Females for Thalassemia")

#Plot to analyse correlation between age and maximum heart rate received vs Heart Condition
ggplot(heart, aes(x = age, y = thalach,color=target1, 
                  size = factor(thalach))) + 
  geom_point(alpha=0.4) + labs(color = "Heart Disease State")+
  guides(size=FALSE) + xlab("Age") + ylab("Maximum Heart Rate Received") + 
  ggtitle("Age vs Maximum Heart Rate Received Separated by Heart Condition")


#Plot to analyse Heart diseases by Exercise induced Angina
ggplot(heart, aes(x= exang, fill=target1)) + 
  geom_bar(position = 'dodge') +
  xlab("Exercise induced Angina") +
  ylab("Count") +
  ggtitle("Analysis of Heart Diseases by Exercise Induced Angina") +
  scale_fill_discrete(name = "Heart Failures", labels = c("Yes", "No"))
