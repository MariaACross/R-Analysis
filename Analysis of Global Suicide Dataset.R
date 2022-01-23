#Final Project 
print("Final Project")

#Installing packages
library("dplyr")
library("tidyverse")
library("countrycode")
library(pastecs)
library(magrittr)
library(plotrix)
library(moments)
library(FSA)
library(summarytools)
library(gmodels)
library(epiDisplay)
library(flexdashboard)
library(highcharter)
library(viridis)
library(DT)
library(corrplot)
library(gtsummary)
library(RColorBrewer)

Suicide_Data<- read.csv("Suicide_Data.csv",header=TRUE, sep=",")
options(max.print=999999)

#Cleaning data
#finding number of missing values for each column
data.frame("column_names"=c(colnames(Suicide_Data)),
           "missing_value_counts"=sapply(Suicide_Data,function(x)sum(is.na(x))),
           row.names=NULL)

#removing columns HDI.for.year and generation
Suicide_Data = subset(Suicide_Data, select=c(-HDI.for.year, -generation))

#replacing null values with 0
Suicide_Data[is.na(Suicide_Data)] <- 0

#changing column names for better understanding
colnames(Suicide_Data) <- c("Country","Year","Sex","Age","Suicides","Population","Suicides_per_100k","Country-year","GDP_for_year","GDP_per_capita")

#removing rows with 2016 data and column Country-year
Suicide_Data <-filter(Suicide_Data, Year != 2016) %>%
  select(-"Country-year")

#using gsub and ifelse to edit data
Suicide_Data$Age <- gsub(' years','',Suicide_Data$Age)
Suicide_Data$Sex <- ifelse(Suicide_Data$Sex == "male", "Male", "Female")

#converting datatypes
Suicide_Data_nominal <- c('country', 'sex', 'continent')

#adding continent column based on country names
Suicide_Data$Continent <- countrycode(sourcevar = Suicide_Data[, "Country"],
                                      origin = "country.name",
                                      destination = "continent")

#converting to factor
Suicide_Data$Country <- as.factor(Suicide_Data$Country)
Suicide_Data$Sex <- as.factor(Suicide_Data$Sex)
Suicide_Data$Continent <- as.factor(Suicide_Data$Continent)


#Create a subset of data for line plot for suicide per 100k for each year in different age groups
suicide_by_age <- Suicide_Data %>%
  select(Year, Age, Suicides, Population) %>%
  group_by(Year, Age) %>%
  summarise(suicide_per_capita = round((sum(Suicides)/sum(Population))*100000, 2))



#line plot for suicide per 100k for each year in different age groups
highchart() %>%
  hc_add_series(suicide_by_age, hcaes(x = Year, y = suicide_per_capita, group = Age),
                type = "line", color = Age) %>%
  hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "",
             pointFormat = paste("Year: <b>{point.x}</b> <br>",
                                 "Age: <b>{point.age}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
  hc_title(text = "Worldwide suicides by Age") %>%
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Suicides per 100K people"),
           allowDecimals = FALSE)

#Global suicides per 100k, by Country
Countries <- Suicide_Data %>%
  group_by(Country, Continent) %>%
  summarize(n = n(),
            Suicides_per_100k = (sum(as.numeric(No_of_suicides)) / sum(as.numeric(Population))) * 100000) %>%
  arrange(desc(Suicides_per_100k))

Countries$Country <- factor(Countries$Country,
                            ordered = T,
                            levels = rev(Countries$Country))
Countries <-head(Countries,50)

ggplot(Countries, aes(x = Country, y = Suicides_per_100k, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Global suicides per 100k, by Country",
       x = "Country",
       y = "Suicides per 100k",
       fill = "Continent") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 50, 2)) +
  theme(legend.position = "bottom")


#Suicide rate by Continent
Suicide_Data %>% 
  group_by(Country, Continent) %>%
  summarize(avg_suicide_rate=mean(Suicides_per_100k)) %>%
  ggplot(aes(Continent, avg_suicide_rate)) +
  geom_boxplot( fill="light blue", color="navy") +
  # coord_flip() +
  labs(x="Continent", y="Suicide reate per 100k population") +
  ggtitle("Suicide rate by Continent")

##Trends By Sex
Suicide_Data%>% group_by(Year, Sex) %>%
  summarize(suicide_rate=sum(Suicides)*100000/sum(Population)) %>%
  ggplot(aes(Year, suicide_rate, col=Sex)) +
  geom_line() + geom_point() +
  facet_grid(Sex ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(1985, 2016, 2)) +
  ggtitle("Suicide Rate Trends by Sex") +
  labs(x="Year", y="Suicide rate per 100k population", col="Sex") +
  scale_color_manual(values = c("darkseagreen", "coral1"),
                     aesthetics = c("colour", "fill")) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") 

#Percentage of Men & Women Committed Suicide
Male <-subset(Suicide_Data$Suicides,Suicide_Data$Sex=="Male")
Male<-sum(Male)
Female<-subset(Suicide_Data$Suicides,Suicide_Data$Sex=="Female")
Female<-sum(Female)
sex<-c(Male,Female)
percentage <-round(100*prop.table(sex),digits=2)


colours <-c("lightblue1","lightcoral")
pie3D(percentage,labels=paste0(percentage,"%"),col=colours,
      main= "Percentage of Men & Women Commited Suicide")
legend("topright", c("Men","Women"), cex = 1,fill = colours)



##Two sample t test based on gdp
summary(Suicide_Data$GDP_per_capita)
low_gdp<- subset(Suicide_Data,GDP_per_capita<16816)
high_gdp<- subset(Suicide_Data,GDP_per_capita>16816)
t.test(low_gdp$Suicides,high_gdp$Suicides,var.equal = TRUE)

## Effect of Nation Wealth on Suicide Rates
Suicide_Data %>% group_by(Country) %>%
  summarize(suicide_rate=sum(Suicides)*100000/sum(Population),
            gdp_per_capita=mean(GDP_per_capita),
            pop=sum(as.numeric(Population))) %>%
  arrange(desc(gdp_per_capita)) %>%
  ggplot(aes(gdp_per_capita, suicide_rate)) +
  geom_point(fill="deepskyblue2", color="navy") +
  stat_smooth(method = "lm", color = "red", size = 1) +
  geom_text(data = . %>% filter(gdp_per_capita>64000 |
                                  suicide_rate>40), aes(gdp_per_capita, suicide_rate, label=Country, col=Country)) +
  labs(x="Average GDP per capita", y="Suicide rate per 100k population") +
  ggtitle("GDP per Capita vs. Suicide Rate") +
  theme(legend.position = "none")

#Trend by Age
age_plot <- Suicide_Data %>% group_by(Age) %>%
  summarize(Suicides_per_100k = (sum(as.numeric(Suicides)) / sum(as.numeric(Population))) * 100000)



ggplot(age_plot, aes(x = Age, y = Suicides_per_100k, fill = Age)) +
  geom_bar(stat = "identity") +
  labs(title = "Global suicides per 100k, by Age",
       x = "Age",
       y = "Suicides per 100k") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

##Two sample t test based on age
summary(Suicide_Data$Age)
Below_55<- subset(Suicide_Data,Age=="15-24" | Age=="25-34"| Age=="5-14" | Age=="35-54")
Above_55<- subset(Suicide_Data,Age=="55-74" | Age=="75+")
t.test(Below_55$Suicides,Above_55$Suicides,var.equal = TRUE)

## t.test for Suicide rate
summary(Suicide_Data$Suicides)
t.test(Suicide_Data$Suicides,mu=243)
sdf <- sd(Suicide_Data$Suicides)
deg_freedom =length(Suicide_Data$Suicides)-1
t <- (mean(Suicide_Data$Suicides)-243)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

# Distribution of suicide_rate
ggplot(Suicide_Data, aes(x = Suicides_per_100k)) +
  geom_histogram(aes(fill = ..count..), binwidth = 10)+
  scale_x_continuous(name = "Suicide rate per 100k ") +
  scale_y_continuous(name = "Count") +
  ggtitle("Distribution of Suicide Rates per 100K") +
  scale_fill_gradient("Count", low = "blue", high = "red")


## Correlation testing to find relation between lung capacity with age and height
Suicide_Data$Sex<-sapply(Suicide_Data$Sex, unclass)
SD1<- Suicide_Data[,c('Suicides','Population','GDP_per_capita','Year','Sex')]
SD1[1:5] <- lapply(SD1[1:5], as.numeric)
sum <- cor(SD1)
round(sum,2)

corrplot(sum, type="upper", order="hclust",
                              col=brewer.pal(n=8, name="RdYlBu"))
corrplot(sum, type="upper", order="hclust", method = 'number')


## linear regression analysis 
Pop <- Suicide_Data$Population - mean(Suicide_Data$Population)
pops<-Suicide_Data$Population/100
mod <- lm(Suicide_Data$Population ~ Pop)
mod
summary(mod)

ggplot(Suicide_Data, aes(x = Suicides, y = pops, color= Suicides))+
  geom_point( aes(fill = Population), size=4, alpha = 0.6) +
  ggtitle("Suicides based on Population") + xlab("Suicides") + ylab("Population")+
  geom_smooth(method = lm, fill='Blue', color='Red',se=TRUE)

## Linear regression for multivariate
reg_model<-lm(Suicides~pops+Sex,Suicide_Data)
reg_model
summary(reg_model)

tbl_regression(reg_model)


