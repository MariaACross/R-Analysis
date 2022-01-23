print("Week 3")

install.packages("ggpubr")
install.packages("zip")
library(ggplot2)
library(zip)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(FSA)
library(pastecs)
library(summarytools)


#Setting working directory & Importing csv file into R script
setwd("C:/Users/admin/Documents")
laptop<- read.csv("laptop_price.csv",header=TRUE, sep=",")
options(max.print=999999)


#Data Cleaning
View(laptop)

#finding number of missing values for each column
data.frame("column_names"=c(colnames(laptop)),
           "missing_value_counts"=sapply(laptop,function(x)sum(is.na(x))),
           row.names=NULL)

#removing unnecessary columns 
laptop = subset(laptop, select=c(-ScreenResolution, -Gpu,-Cpu))

#setting column names
names(laptop)
colnames(laptop) <- c("Laptop_ID","Company","Model","Type","Inches",
                    "RAM","Memory","OS","Weight","Price")

#Statistical Analysis
summary(laptop)
str(laptop)
stat.desc(laptop)
headtail(laptop,5)
fivenum(laptop$Price)
freq(laptop$Company)

#Visual analysis
#Frequency chart of Companies
ggplot(data=laptop, aes(x=Company)) + geom_bar(color='navy',fill='light blue')+
  ggtitle("Count of Laptops by Companies")

#Price Trend
hist(laptop$Price, col="peachpuff", 
     border="black",
     prob = TRUE, 
     xlab = "Price",
     main = "Density of Price of Laptops") 
lines(density(laptop$Price), lwd = 2, 
      col = "chocolate3")

#Price by Companies
ggplot(laptop, aes(x = Company, y = Price, fill=Company))+ 
  geom_boxplot()+ theme(legend.position = "none")

##T and Hypothesis Testing
##Price and Type
ggplot(laptop, aes(x = Type, y = Price, fill=Type))+ 
  geom_boxplot()+scale_fill_brewer(palette="Dark")

#2 in 1 Convertible statistics
Laptop_2n1 <- subset(laptop[c("Price", "Type")], laptop$Type == '2 in 1 Convertible')
meanT <- mean(laptop$Price)
t_test <- t.test(Laptop_2n1$Price, mu = 1124)
t_test
sdf <- sd(Laptop_2n1$Price)
deg_freedom =length(Laptop_2n1$Price)-1
t <- (mean(Laptop_2n1$Price)-1124)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

#Netbook Statistics
Laptop_NB <- subset(laptop[c("Price", "Type")], laptop$Type == 'Netbook')
meanT <- mean(laptop$Price)
t_test <- t.test(Laptop_NB$Price, mu = 1124)
t_test
sdf <- sd(Laptop_NB$Price)
deg_freedom =length(Laptop_NB$Price)-1
t <- (mean(Laptop_NB$Price)-1124)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

#Workstation Statistics
Laptop_WS <- subset(laptop[c("Price", "Type")], laptop$Type == 'Workstation')
meanT <- mean(laptop$Price)
t_test <- t.test(Laptop_WS$Price, mu = 1124)
t_test
sdf <- sd(Laptop_WS$Price)
deg_freedom =length(Laptop_WS$Price)-1
t <- (mean(Laptop_WS$Price)-1124)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

##Price vs OS
ggplot(laptop, aes(x = OS, y = Price, fill=OS))+ 
  geom_boxplot()+scale_fill_brewer(palette="Spectral")

#Linux Statistics
Laptop_Linux <- subset(laptop[c("Price", "OS")], laptop$OS == 'Linux')
meanT <- mean(laptop$Price)
t_test <- t.test(Laptop_Linux$Price, mu = 1124)
t_test
sdf <- sd(Laptop_Linux$Price)
deg_freedom =length(Laptop_Linux$Price)-1
t <- (mean(Laptop_Linux$Price)-1124)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

#Windows10 Statistics
Laptop_Windows <- subset(laptop[c("Price", "OS")], laptop$OS == 'Windows 10')
meanT <- mean(laptop$Price)
t_test <- t.test(Laptop_Windows$Price, mu = 1124)
t_test
sdf <- sd(Laptop_Windows$Price)
deg_freedom =length(Laptop_Windows$Price)-1
t <- (mean(Laptop_Windows$Price)-1124)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

#mac Statisctics
Laptop_mac <- subset(laptop[c("Price", "OS")], laptop$OS == 'macOS')
meanT <- mean(laptop$Price)
t_test <- t.test(Laptop_mac$Price, mu = 1124)
t_test
sdf <- sd(Laptop_mac$Price)
deg_freedom =length(Laptop_mac$Price)-1
t <- (mean(Laptop_mac$Price)-1124)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

#Windows10S Statistics
Laptop_Windows10S <- subset(laptop[c("Price", "OS")], laptop$OS == 'Windows 10 S')
meanT <- mean(laptop$Price)
t_test <- t.test(Laptop_Windows10S$Price, mu = 1124)
t_test
sdf <- sd(Laptop_Windows10S$Price)
deg_freedom =length(Laptop_Windows10S$Price)-1
t <- (mean(Laptop_Windows10S$Price)-1124)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

##Two sample t test
mod_laptops<- subset(laptop,Company=='Acer'|Company=='Asus'|Company=='Google')
famous_laptops<-subset(laptop,Company=='Dell'|Company=='Lenovo'|Company=='HP')
t.test(mod_laptops$Price,famous_laptops$Price,var.equal = TRUE)

# t.test
summary(laptop$Price)
t.test(laptop$Price,mu=1124)

##Hypothesis Testing
#calculating Standard Deviation
sdf <- sd(laptop$Price)
sdf

#Calculating Degrees of Freedom
deg_freedom =length(laptop$Price)-1
deg_freedom

#Calculating t Score
t <- (mean(laptop$Price)-1124)/(sdf/sqrt(deg_freedom))
t

#Calculating p value
p_value<-2*pt(-abs(t),deg_freedom)
p_value



