---
  title: "Data Visualisation and Analytics: Exam Template"
author: "27317501"
output: pdf_document
---
library(MASS)
library(tidyverse)
library(ggthemes)
library(mvtnorm)
library(kableExtra)
library(rpart)
library(rpart.plot)
library(ca)
library(knitr)
library(kableExtra)
library(dplyr)
library(stats)
library(broom)
library(mclust)
library(ggplot2)
library(readr)
library(rvest)
library(GGally)
library(ggplotlyExtra)
library(class)

# Do not change this number/line:
set.seed(17895)

# Lastly load in your datasets here:
myID=27317501
data<-read_csv('data27317501.csv')
dataTS<-read_csv('dataTS27317501.csv')
```


# Visualization Control (10 Marks)
**This question should only be answered if you are enrolled in ETF5922**
  
  *This section is based on the main dataset.*
  
##1. Create a scatter plot with debt as a proportion of GDB on the x axis and inflation rate on the y axis using the default settings.* **(1 Mark)**
  
ggplot(data,aes(y=InflationRate,x=DebtGDP))+
  geom_point()


##2. Change the points to text corresponding to the country name, and colour the text according to the credit rating.* **(2 Marks)**
  
ggplot(data,aes(x=InflationRate,y=DebtGDP,label=Country,col=CreditRating))+
  geom_text()
```

##4. Create a bar chart to represent the average per capita GDP in different credit ratings* **(2 Marks)**

data1<-data%>%
  group_by(CreditRating)%>%
  dplyr::select(PerCapitaGDP)%>%
  summarise(AverageGDP=mean(PerCapitaGDP),na.rm=T)
data1
ggplot(data1,aes(x=AverageGDP))+
  geom_bar()


##5. Now change the bar chart from 4 to represent the average per capita GDP in different credit ratings, facetted by the largest industry in the country. * **(1 Mark)**
  
data1<-data%>%
  group_by(CreditRating,LargestIndustry)%>%
  dplyr::select(PerCapitaGDP)%>%
  summarise(AverageGDP=mean(PerCapitaGDP),na.rm=T)

ggplot(data1,aes(x=AverageGDP))+
  geom_bar()+facet_wrap(~LargestIndustry)


##6. Now add an appropriate x and y axis to the plot from 5. Colour the bars by credit rating, but do not include a legend. Add an appropriate theme to the figure, and a figure caption. * **(2 Marks)**

ggplot(data1,aes(x=AverageGDP,col=CreditRating))+
  geom_bar()+facet_wrap(~LargestIndustry)+theme_economist()


# Data Munging (15 Marks)

##*This section is based on the main dataset.*
  
##1. Find the number of countries that have each level of credit rating, i.e. the number of countries with a A bond rating, number of countries with a B bond rating, etc.  Present these in a dataframe.* **(2 Marks)**

data%>%
  filter(CreditRating=='A')%>%
  dplyr::select(Country)%>%
  summarise(Abond=n())
data%>%
  filter(CreditRating=='B')%>%
  dplyr::select(Country)%>%
  summarise(Bbond=n())
  
##4. What is the average Debt to GDP ratio for countries for the countries with a credit rating equal to your answer to Question 3.  Round your answer to the nearest unit.* **(2 Marks)**

data%>%
  filter(CreditRating=='A')%>%
  dplyr::select(DebtGDP)%>%
  summarise(Average_A=mean(DebtGDP),na.rm=T)


##5. How does your answer to Question 2 change if countries with a Debt to GDP ratio higher than your answer to Question 4 are excluded from the sample.*  **(2 Marks)**

data_DebtGDP<-data%>%filter(DebtGDP<=84.88759)
data_DebtGDP%>%
  filter(CreditRating=='A')%>%
  summarise(CountA_new1=n())


##6. How does your answer to Question 2 change if countries with a Debt to GDP ratio higher than your answer to Question 4 AND countres for which Agriculture is the largest industry are excluded from the sample.* **(2 Marks)**

data_DebtGDP_new<-data%>%filter((DebtGDP<=84.88759)&(LargestIndustry!='Agriculture'))
data_DebtGDP_new%>%
  filter(CreditRating=='A')%>%
  summarise(CountA_new2=n())

##7. Per Capita Debt can be found by multiplying per capita GDP by debt to GDP ratio and dividing by 100.  Which country has the highest value of per capita debt?*  **(3 Marks)**
data%>%
  group_by(Country)%>%
  mutate(PerCapitaDebt=(PerCapitaGDP*DebtGDP)/100)%>%
  summarise(Highest=max(PerCapitaDebt))%>%
  print()
data_highest<-data%>%
  mutate(PerCapitaDebt=(PerCapitaGDP*DebtGDP)/100)%>%
  summarise(Highest=max(PerCapitaDebt))%>%
  print()

##8. What is the lowest per capita debt for countries that have either a Manufacturing as the largest industry or a B credit rating?* **(2 Marks)**

data_lowestest<-data%>%
  mutate(PerCapitaDebt=(PerCapitaGDP*DebtGDP)/100)%>%
  summarise(Lowest=min(PerCapitaDebt))%>%
  print()

# Advanced Visualisation (15 Marks)
##1. Construct a scatterplot of debt to GDP ratio (x axis) against per capita GDP (y axis).  Present information about the credit rating in color using the default viridis colormap.  Allow the size of each point to depend on the value of the exports to GDP ratio.  Facet by largest industry using a 2x2 grid.  Each facet should have its own unique y axis.  Add a linear regression line (without confidence intervals) to each subplot.  There should be single regression line in each facet.  These regression lines should have the a color with hex code #FF0000.* **(12 Marks)**

ggplot(data,aes(x=DebtGDP,y=PerCapitaGDP,col=CreditRating,size=ExportsGDP))+
  geom_point()+scale_color_viridis_d()+
  facet_wrap(~LargestIndustry,scales='free_y',nrow=2,ncol=2)+
  geom_smooth(method='lm',color='#FF0000')


# Discriminant Analysis (10 Marks)
##1. Construct a linear discriminant analysis using default values of the `lda` function with the training data only, with credit rating as the target variable and all other variables (excluding Country Name, largest industry and TestTrain) as predictors.* **(1 Mark)**

train_data<-data%>%
  filter(TrainTest=='Train')%>%
  dplyr::select(PerCapitaGDP,DebtGDP,ExportsGDP,InflationRate,CreditRating,-TrainTest)

ldaout<-lda(CreditRating~.,data = train_data)
yhat_lda<-predict(ldaout,train_data)


##2. Compute the training misclassification rate.* **(2 Marks)**

mean(yhat_lda$class!=train_data$CreditRating)


##3. Compute the test misclassification rate.* **(1 Marks)**

test_data<-data%>%
  filter(TrainTest=='Test')%>%
  dplyr::select(PerCapitaGDP,DebtGDP,ExportsGDP,InflationRate,CreditRating,-TrainTest)

ldaout<-lda(CreditRating~.,test_data)
yhat_lda<-predict(ldaout,test_data)
mean(yhat_lda$class!=test_data$CreditRating)


##4. Consider a country with a per capita GDP of $1083.29, a debt to GDP ratio of 91.48 percent, an exports to GDP ratio of 68.21 percent, an inflation rate of 3.873 percent and a largest industry of Manufacturing.  What is the probability of each credit rating?* **(1 Mark)**

rpart1<-data%>%dplyr::select(PerCapitaGDP,DebtGDP,ExportsGDP,InflationRate,CreditRating,LargestIndustry,-Country,-TrainTest)
tree<-rpart(formula = CreditRating ~ .,rpart1)
rpart.plot(tree)

##5. Calculate the variance of per capita GDP, debt as a proportion of GDB, exports as a proportion of GDP and inflation rate for each credit rating in the training data.* **(1 Mark)**

train_data%>%group_by(CreditRating)%>%
  summarise(VarPerCapitaGDP=var(PerCapitaGDP),VarDebtGDP=var(DebtGDP),covPerCapitaGDP_DebtGDP=cov(PerCapitaGDP,DebtGDP))->varcov
varcov

##7. Using the default settings of the `qda` function, demonstrate whether this is the case for this dataset.* **(2 Marks)**

qdaout<-qda(CreditRating~.,test_data)
yhat_qda<-predict(qdaout,test_data)
mean(yhat_qda$class!=test_data$CreditRating)


# Time Series Visualisation (10 Marks)

##1. Plot the Sales for Mac and the Sales for PC over time using a single plot (i.e. same x axis and y axis). Plot each line using a different colour with a colorblind colour scheme* **(6 Marks)**

dataTS%>%
  pivot_longer(col=-Year)%>%
  ggplot(aes(x=Year,y=value,color=name))+
  scale_color_viridis_d()+
  geom_line()

##2. Construct a path plot for sales of both computer brands with Mac on the x axis and PC on the y axis.  Use the viridis color scale.  Add the years onto the plot (in black) as text.* **(4 Marks)**

dataTS%>%
  pivot_longer(col=-Year)%>%
  ggplot(aes(x=Year,y=value,color=name,label=Year))+
  scale_color_viridis_d()+
  geom_path()+
  geom_text()

