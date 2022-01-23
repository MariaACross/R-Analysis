#Week 1- module 1
#import the data set
stroke =  read.csv("stroke-data.csv")

#datacleaning
names(stroke)<- c('id','gender','age','hypertension','heart_disease','ever_married', 
                  'work_type','Residence_type','avg_glucose_level','bmi','smoking_status','stroke')
stroke$stroke<- factor(stroke$stroke, levels = c(0,1), labels = c("No", "Yes"))
stroke$gender<-as.factor(stroke$gender)
stroke$hypertension<- factor(stroke$hypertension, levels = c(0,1), labels = c("No", "Yes"))
stroke$heart_disease<- factor(stroke$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
stroke$work_type<-as.factor(stroke$work_type)
stroke$Residence_type<-as.factor(stroke$Residence_type)
stroke$smoking_status<-as.factor(stroke$smoking_status)
stroke$bmi<-as.numeric(stroke$bmi) 


#Week 2- Part1

#installing and loading necessary packages
install.packages("psych")
install.packages("epiDisplay")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("ggplot2")
library(psych)
library(ggplot2)
library(epiDisplay)
library(pastecs)
library(Hmisc)
library(dplyr)
#Calculating Descriptive Statistics
mean(stroke$age)
median(stroke$age)
sd(stroke$age)
min(stroke$avg_glucose_level)
max(stroke$avg_glucose_level)
mean(stroke$avg_glucose_level)

#Computing two new attributes Male and Female data
male_stroke = stroke[stroke$gender=="Male",]
female_stroke = stroke[stroke$gender=="Female",]

#Analysing how data varies between Males and Females
mean(male_stroke$bmi, na.rm = TRUE)
mean(female_stroke$age)
mean(male_stroke$avg_glucose_level)
mean(female_stroke$avg_glucose_level)
max(male_stroke$avg_glucose_level)
max(female_stroke$avg_glucose_level)

install.packages("data.table")
library(data.table)
install.packages("summarytools")
library(summarytools)
table<-aggregate(x = stroke$stroke,                
               by = list(stroke$gender),              
              FUN = c("count","sum","mean","median","sd","se","min","max"))
#pastecs::stat.desc()
stat.desc(stroke)

#stats::fivenum()
fivenum(stroke$age)

#psych::describe( ) to analyse the dataset
describe(stroke)

#creating sub class tables
smk1<-stroke %>% filter(smoking_status== "formerly smoked")  
psych::describe(smk1)

smk2<-stroke %>% filter(smoking_status== "never smoked")  
psych::describe(smk2)

smk3<-stroke %>% filter(smoking_status== "smokes")  
psych::describe(smk3)  

smk4<-stroke %>% filter(smoking_status== "Unknown")  
psych::describe(smk4) 


#aggregate function to Split the numeric variable into subsets and
#compute summary statistics for each 
aggregate(x = stroke$avg_glucose_level,                
          by = list(stroke$heart_disease),              
          FUN = mean) 
aggregate(x = stroke$avg_glucose_level,                
          by = list(stroke$smoking_status),              
          FUN = mean) 

aggregate(x = stroke$avg_glucose_level,                
          by = list(stroke$stroke),              
          FUN = mean) 

aggregate(x = stroke$bmi,                
          by = list(stroke$stroke),              
          FUN = mean)

aggregate(x = stroke$age,                
          by = list(stroke$stroke),              
          FUN = mean) 

 
#3 line Table
install.packages("bruceR")
library(bruceR)

mid_age <- data.frame(c(filter(stroke, age ==50,bmi<=25)))
df2 <- subset(baby,select=c(hypertension,bmi,stroke))
print_table(df2,  row.names = TRUE, col.names = TRUE,
  title = "Parameters of Patients aged 50", note = "", append = "",
  line = TRUE,file = NULL,
  file.align.head = "auto",file.align.text = "auto")


##Week 2- Part 2
#Bar plot- Stroke by Workplot
ggplot(stroke, aes(x = work_type, fill = stroke))+
  geom_bar(position = "fill")+ggtitle("Occurance on Stroke by Work types")+
  stat_count(geom = "text",
             aes(label = stat(count)),
             #position = "fill", color = "black"
             position = position_fill(vjust = 0.5), color = "black")

#Barplot- Correlation between Residence Type, Glucose Levels, Smoking Status and Stroke
ggplot(stroke, aes(x = stroke, y = avg_glucose_level))+
  ggtitle("Correlation between Residence Type, Glucose Levels, 
          Smoking Status and Stroke")+
  geom_bar(aes(fill = Residence_type),stat = "identity", 
           position = position_dodge(0.9))+
  facet_wrap(~smoking_status, scales = "free")

#Mosaicplot- Stroke in Males by Worktype
mosaicplot(table(male_stroke$stroke, male_stroke$work_type), 
           col=c("#4682B4", "#B4464B", "#B4AF46"), 
           las=1, main="Stroke in Males by Work Type")

#BOXPLOT- Correlation between Heart disease, BMI, Gender and Stroke
ggplot(stroke, aes(x = stroke, y = bmi, fill = heart_disease, color = gender))+
  geom_boxplot(alpha = 0.5) +ggtitle("Correlation between Heart disease, BMI, 
                                     Gender and Stroke")+
  geom_point(alpha = 0.5, position = position_jitterdodge(jitter.width = 0.1, 
                                                          jitter.height = 0.1))

#Boxplot- Stroke vs Glucose Level vs Hypertension
ggplot(stroke, aes(x = stroke, y = avg_glucose_level, color = hypertension)) +
  geom_boxplot() + xlab("Occrance of Stroke") + 
  ylab("Average Glucose Level") + 
  ggtitle("Relation between Glucose Level,Hypertension and Stroke")+
  geom_point(alpha = 0.5, position = position_jitterdodge(jitter.width = 0.1, 
                                                          jitter.height = 0.1))

#Jitter Chart- Stroke vs age
ggplot(stroke, aes(x=age, y=stroke)) + 
  geom_jitter(position=position_jitter(0.2), shape=17,cex=1.2,color='light blue')+
  ggtitle("Correlation between Stroke and Age")+
  stat_summary(fun=mean, geom="point", shape=18,
  size=3, color="red")

#Box+jitter- Stroke vs BMI
ggplot(smk3, mapping = aes(x= stroke, y= bmi, fill= stroke))+
  geom_boxplot()+geom_jitter(size=1.2,width = 0.3,color='seagreen')+ 
  ggtitle("Stroke vs BMI")

#Scatter Plots with abline and par
par(cex= 1,mai=c(0.9,0.9, 0.5, 0.5))
plot(smk1$age,smk1$avg_glucose_level,
     main="Variation of Glucose Levels with Age for Formerly smoked", 
     ylab = "Average Glucose Level", xlab = "Age", col='light blue',pch=10)
abline(lm(avg_glucose_level~age, data=stroke), col='coral')

par(cex= 1,mai=c(0.9,0.9, 0.5, 0.5))
plot(smk2$age,smk2$avg_glucose_level,
     main="Variation of Glucose Levels with Age Never Smoked", 
     ylab = "Average Glucose Level", xlab = "Age", col='light pink',pch=08)
abline(lm(avg_glucose_level~age, data=stroke), col='sea green')

par(cex= 1,mai=c(0.9,0.9, 0.5, 0.5))
plot(smk3$age,smk3$avg_glucose_level,
     main="Variation of Glucose Levls with Age for Smokers", 
     ylab = "Average Glucose Level", xlab = "Age", col='gold',pch=20)
abline(lm(avg_glucose_level~age, data=stroke), col='coral')
