#=========================================================
# IDS 462, Session 7
# Midterm Review Session 
#=========================================================
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================


# Instructions 
#=============
# An RData file has been prepared for you. The file consists of data you already know, the realestate data, and a new data frame, credit. 
# Answer each one of the questions assigned to your group *on your own* -- a total of three questions. You have 1.25 hour. 
# Discuss your solutions as a group. You have 0.5 hour, including a 10 min. break. 
# Present solutions to class. Discuss issues/concerns. Each group gets 15 min.  

# Tips: 
# Make sure to properly allocate time! Start by exploring the new data frame and consider how to best answer these questions. 


# Group I 
#========

# realestate data 
#=
# Is there a difference in the distribution of air conditioning by bedrooms as a factor? Use relevant statistics, plots and statistical tests. For your plots, use at least one ggplot (properly titled, annotated, and visually reasonable). Detail your findings.

# Common question: Which variables in the data in your view exhibit the strongest relationship with price? Provide evidence and explain your answer. 

# credit data 
#=
# *Throughly* examine the relationships of Income, Balance, Age, Gender, Ethnicity (all of these are potential IVS), with Rating (our DV). Which of these IVs are good predictors? Use statistics, plots, and tests, and provide a detailed answer. 


# Group II  
#=========

# realestate data 
#=
# What is the relationship between price and lotsize. Use relevant statistics, plots and statistical tests. For your plots, use at least one ggplot (properly titled, annotated, and visually reasonable). Detail your findings.

# Common question: Which variables in the data in your view exhibit the strongest relationship with price? Provide evidence and explain your answer. 

# credit data 
#=
# *Throughly* examine the relationships of Limit, Cards, Education, Student, Married (all of these are potential IVS), with Rating (our DV). Which of these IVs are good predictors? Use statistics, plots, and tests, and provide a detailed answer.   

load("Session 7 (review).RData")
View(realestate)
glimpse(realestate)
colSums(is.na(realestate))
summary(realestate$lotsize)
summary(realestate$price)
options(scipen=99) 

#Question 1

#univariate analysis
summary(realestate$lotsize)
summary(realestate$price)
outlier_values <- boxplot.stats(realestate$lotsize)$out
outlier_values
outlier_values1<-boxplot.stats(realestate$price)$out1
outlier_values1
realestate <- realestate[-c(outlier_values,outlier_values1),] 

ggplot(data=realestate) + aes(x=lotsize, y=price) +
  geom_point(pch=16, color="coral") +
  labs(title='Relationship between price and lotsize',
       x="lotsize", y="price") + # x for xlab, y for ylab 
  geom_smooth(method="lm", color="black", lwd=2)

cor.test(realestate$lotsize, realestate$price)
#there is relationship between lotsize and price. Positive relationship. As the lotsize increases price increases

#Question 2
mod1<-lm(price ~ lotsize, data=realestate)
mod1
summary(mod1) 
# we can also set confidence interval at 99% 
confint(mod1, level=0.99)

 
predict(mod1 , data.frame(lotsize =(c(4000 ,10000 ,12000) )),
        interval ="confidence", level=0.99) 
boxplot(residuals(mod1))
plot(realestate$price~realestate$lotsize, pch=16, col="lightblue")
abline(mod1, col="red", lwd=3)
#there is a positive relationship between lotsize and price. 

mod3<-lm(realestate$price~realestate$lotsize+realestate$bedrooms+realestate$bathrms+realestate$stories+realestate$driveway+realestate$recroom+realestate$fullbase+realestate$airco+realestate$gashw+realestate$garagepl+realestate$prefarea)
summary(mod3)
plot(realestate$price~realestate$lotsize+realestate$bedrooms+realestate$bathrms+realestate$stories+realestate$driveway+realestate$recroom+realestate$fullbase+realestate$airco+realestate$gashw+realestate$garagepl+realestate$prefarea)
#the variable that exhibit strongest relationship are lotsize,bathrooms,stroies,fullbaseyes,aircoyes,gashwyes,garagepl2

#question 3

colSums(is.na(credit))
glimpse(credit)
credit$Income <- as.numeric(credit$Income)
credit$Limit<-as.numeric(credit$Limit)
gsub("\\$","",credit)
summary(credit$Limit)
credit1<-credit
credit1<-na.omit(credit1)
View(credit1)
summary(credit1)
#check the distribution,multicollinearity

mod2<-lm(Rating~Limit+Cards+Education+Student+Married,data=credit)
mod2
Predictions<-predit.lm(mod2,credit1)
predict(mod2,credit1)
summary(mod2)
boxplot(residuals(mod2))
plot(credit1$Rating~credit1$Limit+credit1$Cards+credit1$Education+credit1$Student+credit1$Married, pch=16, col="lightblue")
anova(mod2)
#correlation test
#The independent variable that is significant is Limit to predit the rating.
