#=================================
# IDS570 Statistics for Management
# PROJECT ON UDEMY COURSE ANALYSIS
# TEAM 2
# GROUP MEMBERS: AKANKSHA JAIN
#                DELISHYA MATHEW
#                HARSH MISRA
#=================================


###########################
#Load necessary Libraries:
###########################

library(car)
library(dplyr)
library(psych)
library(dplyr)
library(gdata)
library(plyr)
library(corrplot)
library(gmodels)
library(ggplot2)
library(gplots)
library(RColorBrewer)


#######################
# Load Udemy Data File: 
#######################

load("finalreport.Rdata") 


#######################
# Data Exploration: 
#######################

str(udemyOriginal) # Initial Class of all data Variables


#######################
# Dataset Cleaning:
#######################

# Cleaning of Dataset and conversion of columns into relevant datatypes.

udemy$price <- as.character(udemy$price)        # Changing Price to Charactor
udemy$price <- gsub("Free", "0", udemy$price)   # Converting price from "Free" to 0 for numeric analysis

# Converting variables to relevant datatypes:

udemy$price <- as.numeric(udemy$price)          # Changing Price to Numeric   
udemy$id <- as.character(udemy$id)              # Changing Id to Character
udemy$title <- as.character(udemy$title)        # Chaning Title to Character
udemy$url <- as.character(udemy$url)            # Chaning url to Character

# Factor Conversion:

udemy$isPaid <- as.factor(udemy$isPaid)         # Changing isPaid to Factor
table(udemy$isPaid)
udemy$instructionalLevel <- as.factor(udemy$instructionalLevel) # Changing Instruction Level to Factor
table(udemy$instructionalLevel)
udemy$Category <- as.factor(udemy$Category)     # Changing Category to Factor
table(udemy$Category)

# Converting unit of time in "ContentInfo" to Hours (Minutes and Hours earlier)

udemy$y <- gsub("hour|hours", " ", udemy$contentInfo) # Replace hours or hour as text to blank
udemy$y <- as.numeric(udemy$y)
udemy$x <- gsub("min | mins", " ", udemy$contentInfo) # Replace mins or min as text to blank
udemy$x <- as.numeric(udemy$x)
udemy$x <- udemy$x/60                                 # Converting each row to Hours
format(round(udemy$x, 2), nsmall = 2)                 # Rounding to 2 decimal places

# Concatenating both Columns for minutes and seconds

udemy$duration <- paste(udemy$y, udemy$x)
udemy$duration <- gsub("NA", " ", udemy$duration)
udemy$duration <- gsub(" ", "", udemy$duration)
udemy$duration <- as.numeric(udemy$duration)          # converting Duration of course to Numeric.

# Changing levels in Factor Variables for Hypothesis Testing:

udemy$year <- substring(udemy$publishedTime,1,4)      # Extracting Year from Published Date Column:

# Classifing courses based on published year (Level: Old, Morderate and New)

udemy$yearclass[udemy$year == '2011' | udemy$year == '2012']<-"Old" 
udemy$yearclass[udemy$year == '2013' | udemy$year == '2014']<-"Morderate"  
udemy$yearclass[udemy$year == '2015' | udemy$year == '2016' | udemy$year == '2017'] <- "New"
udemy$yearclass <- as.factor(udemy$yearclass)

# Classifying course based on instruction level (Level: Expert and Other)

udemy$courseLevel[udemy$instructionalLevel == 'Expert Level']<-"Expert" 
udemy$courseLevel[udemy$instructionalLevel != 'Expert Level']<-"Other"
udemy$courseLevel <- as.factor(udemy$courseLevel)

# Deleting Columns not required for analysis:

udemy$X.1 <- NULL
udemy$X <- NULL
udemy$Total <- NULL
udemy$y <- NULL
udemy$x <- NULL
udemy$contentInfo <- NULL
udemy$Is.Paid <- NULL
udemy$Percent <- NULL

#Checking for NA's and omitting them:

table(is.na(udemy$duration))
udemy <- na.omit(udemy)
table(is.na(udemy$duration))

# Checking for proper datatype classification:
str(udemy)

#######################
# Univariate Analysis: 
#######################

## numSubscribers[DEPENDENT VARIABLE] : Subscribers for a particular course (Numeric)
options(scipen = 99)
hist(udemy$numSubscribers, main="Distribution of Number of Subscribers", 
     col='indianred3', freq=F, xlab = "Number of Subscribers",xlim=c(0,200000), breaks = 100)
lines(density(udemy$numSubscribers, na.rm = T), col="lightblue", lwd=3)

hist(log(udemy$numSubscribers), main="Distribution of No Of Subscribers", 
     col="indianred3", freq=F, xlab = "Log of Number of Subscribers")
lines(density(log(udemy$numSubscribers), na.rm = T), col="lightblue", lwd=3)

# Initially Right Skewed distribution
# Obtained a normalised graph for the number of subscribers after log transformation, 
# which was used to find relation between other variables and number of subscribers for our analysis 

## isPaid: Course is Paid or not (Factor)
tab0 <-table(udemy$isPaid)
tab0
barplot(tab0,ylim=c(0,4000), axes=F, col='indianred3', main="Course distribution as Paid/Unpaid")
axis(side = 2, at = seq(from=0, to=4000, by=500))
box(lwd=1.5)

ggplot(udemy, aes(x=isPaid, fill= isPaid)) + 
  geom_bar(position="dodge") + 
  labs(title="Course distribution as Paid/Unpaid", x="isPaid", y="Count of Courses") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "isPaid")

# The courses which are paid are more in number as compared to the courses which are free of cost.

## price : Price of a course at Udemy (Numeric)
hist(udemy$price, main="Distribution of Price", 
     col="indianred3", freq=F, xlab = "Price",xlim=c(0,200))
lines(density(udemy$price, na.rm = T), col="lightblue", lwd=3)

# The Plot is multimodal with many outliers

hist(log(udemy$price), main="Distribution of Price", 
     col="indianred3", freq=F, xlab = " Log of Price")
lines(density(log(udemy$price), na.rm = T), col="lightblue", lwd=3)

# Log transformation did not result in normalized distribution for price 

hist(sqrt(udemy$price), main="Distribution of Price", 
     col="indianred3", freq=F,ylim=c(0,0.30), xlab = "Sqaure Root of Price")
lines(density(sqrt(udemy$price), na.rm = T), col="lightblue", lwd=3)

# Sqrt transformation did not normalized the plot

## numReviews: Reviews for a particular course (Numeric)
hist(udemy$numReviews, main="Distribution of Number of Reviews", 
     col="indianred3", freq=F, xlab = "Number of Reviews")
lines(density(udemy$numReviews, na.rm = T), col="lightblue", lwd=3)

hist(log(udemy$numReviews), main="Distribution of Number of Reviews", 
     col="indianred3", freq=F, xlab = " Log of Number of Reviews")
lines(density(log(udemy$numReviews), na.rm = T), col="lightblue", lwd=3)

# Right Skewed distribution for number of reviews
# Log tranformation normalized the distribution for number of Reviews

## NumPublishedLectures: Lectures published for a particular course (Numeric)
hist(udemy$numPublishedLectures, main="Distribution of Number of Published Lectures", 
     col="indianred3", freq=F, xlab = "Number of Published Lectures", ylim= c(0, 0.025))
lines(density(udemy$numPublishedLectures, na.rm = T), col="lightblue", lwd=3)

hist(log(udemy$numPublishedLectures), main="Distribution of Number of Published Lectures", 
     col="indianred3", freq=F, xlab = "Log of Number of Published Lectures", ylim=c(0,0.50))
lines(density(log(udemy$numPublishedLectures), na.rm = T), col="lightblue", lwd=3)

# Right Skewed distribution for number of Published Lectures
# Log tranformation normalized the distribution for number of Published Lectures

## instructionLevel: Level of a course at Udemy (Factor)
tab1 <-table(udemy$instructionalLevel)  # Stores factor levels for instruction level in tabular form.
tab1
barplot(tab1, ylim=c(0,2000),col='indianred3', main="Course distribution as per Instruction level")
box(lwd=1.5)  # Maximum values for all level courses.

# ggplot for the same variable: 
ggplot(udemy, aes(x=instructionalLevel, fill= instructionalLevel)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Instructional Level", x="Instructional Level", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Courses per Instruction level")

# More number of courses in 'All Levels' followed by "Beginner Level".

## Duration: Duration of a course in Hours (Numeric)
hist(udemy$duration, main="Course Duration", 
     col="indianred3", freq=F, xlab = "Course Duration",ylim=c(0,0.30))
lines(density(udemy$duration, na.rm = T), col="lightblue", lwd=3)

hist(log(udemy$duration), main="Distribution of CourseDuration", 
     col="indianred3", freq=F, xlab = "Log of Course Duration")
lines(density(log(udemy$duration), na.rm = T), col="lightblue", lwd=3)

# Right skewed distribution for Course Duration

## Category: Category of a course at Udemy (Factor)
tab2 <- table(udemy$Category) # # Shows factor levels for Category in tabular form.
barplot(tab2,ylim=c(0,1500),col='indianred3', main="Course distribution as per Category")
box(lwd=1.5)

ggplot(udemy, aes(x=Category, fill= Category)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Category", x="Category", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Category")

# More number of courses in Web Development and Business Finance


# Summary of Univariate Analysis

# Majority of the variables are right-skewed. We used logarithmic transformation to transform them.
# Number of Subscribers, Price, isPaid, Category, Instruction Level and Course duration are important 
# variables for further analysis.


#####################
# Bivariate Analysis:
#####################


## Distribution of Subscribers across Instruction level (Numeric and Factor Analysis)
describeBy(log(udemy$numSubscribers+1) , udemy$instructionalLevel) # Statistical Analysis of subscribers across Instruction Level.
boxplot(log(numSubscribers+1)~instructionalLevel, data=udemy, col=brewer.pal(10, "Set3"), main="Number Of Subscribers across Instruction Level")
# More number of Subscribers for 'All Levels' and 'Beginner Level'

##Distribution between Num of Subscribers across Category (Numeric and Factor Analysis)
describeBy(udemy$numSubscribers , udemy$Category) # Statistical Analysis of subscribers across Instruction Level.
boxplot(log(numSubscribers+1)~Category, data=udemy, col=brewer.pal(10, "Set3"), main="Number Of Subscribers across Category")
# Number of Subscribers maximum for Web Development course followed by Business Finance

##Distribution between Price and Instruction Level (Numeric and Factor Analysis)
describeBy(udemy$price , udemy$instructionalLevel) 
boxplot(log(price+1)~instructionalLevel, data=udemy, col=brewer.pal(10, "Set3"), main="Price of Courses across Intruction Level" )
# Expert level courses are of lower price. All Level and Beginner Level are expensive

##Distribution between Price and Category (Numeric and Factor Analysis)
describeBy(udemy$price , udemy$Category)
boxplot(log(price+1)~Category, data=udemy, col=brewer.pal(10, "Set3"), main="Price of Courses across Category")
# Compared to other categories Web development has higher price.

## Distribution of Levels across Category (Factor and Factor Analysis)
tab3 <- table(udemy$instructionalLevel, udemy$Category)
barplot (tab3, main= "Distribution of Levels across Category", col=brewer.pal(10, "Set3"))
legend("top", 
       legend = rownames(tab3), 
       fill = brewer.pal(10, "Set3"))
# There are more courses in All Level and Beginner Level 

## Distribution of Instruction Level across Paid/unpaid
tab4 <- table(udemy$instructionalLevel, udemy$isPaid)
barplot(tab4, main = "Distribution of Paid/Unpaid Courses across Instruction Level", col=brewer.pal(10, "Set3"))
legend("topleft", 
       legend = rownames(tab3), 
       fill = brewer.pal(10, "Set3"))
# Majority of courses are  Paid in Beginner and All Level courses

# From the abiove bivariate analysis we conclude few important results:
# 1: There are more subscribers for All level and beginner level courses.
# 2: Most number of subscriber for web development courses than any other courses.
# Based on these we will form our hypothesis.


#####################
# Hypothesis Testing: 
#####################

# Hypothesis 1:The Average Number of subscribers is less for expert level courses as compared to other levels.

mean(udemy$numSubscribers[udemy$courseLevel=="Other"])
options(scipen = 99)
## T Test:

numSubs_null <- t.test(udemy$numSubscribers[udemy$courseLevel=="Expert"], alternative="less", mu=3230.966, conf.level=0.95)
numSubs_null
SumSubs_hypothesis_1 <- t.test(udemy$numSubscribers[udemy$courseLevel=="Expert"],udemy$numSubscribers[udemy$courseLevel=="Other"])
SumSubs_hypothesis_1
#We reject the null.

#Hypothesis 2: The Average Number of Subscribers is more for Web Development courses than other courses.

# Checking for conditions of ANOVA
aggregate(udemy$numSubscribers ~udemy$Category, FUN="mean") 
plotmeans(udemy$numSubscribers~udemy$Category, xlab="Category", ylab="Number of Subscribers", lwd=3,col=3, p=0.99)

## ANOVA Test:

udemy.aov <- aov(udemy$numSubscribers ~udemy$Category, data=udemy) # Number of subscribers.
summary(udemy.aov)
udemy.tk<-TukeyHSD(udemy.aov) # Tukey test for further analysis
udemy.tk
# We reject the null, there is a dependency of number of subscribers on Category.

#Hypothesis 3: Mean price is same across all course categories.

aggregate(udemy$price ~udemy$Category, FUN="mean") 
plotmeans(udemy$price~udemy$Category, xlab="Category", ylab="Price", lwd=3, col="red", p=0.99)

## ANOVA Test:
udemy.aov1 <- aov(udemy$price ~udemy$Category, data=udemy)
summary(udemy.aov1)
udemy.tk1<-TukeyHSD(udemy.aov1)
udemy.tk1
# We reject the null.

# Hypothesis 4: #Category and number of courses. We reject the null , so there has been a difference in number of courses over the years for each ctegory

udemytab<- table(udemy$Category, udemy$yearclass)
addmargins(udemytab,c(1,2))
pudemytab<-prop.table(udemytab)
round(pudemytab,2)
addmargins(round(pudemytab,2),c(1,2))
mosaicplot(pudemytab, main='Category and Number of courses', col=brewer.pal(10, "Set3"))

## Chisq Test:
chisq.test(udemytab)
CrossTable(udemy$Category, udemy$yearclass, 
           prop.c = F, prop.r = F, expected=T, 
           dnn = c('actual outcome', 'expected outcome'), format=c("SPSS"), chisq = T)

# We reject the null.


#####################
# Correlation: 
#####################

#correlation matrix: Number of Subscribers and its correlation with other Variables
udemynum <-udemy[ ,c(5,6,7,8,12)]
cormat <- cor(udemynum)
cormat
corrplot(cormat,  addCoef.col = "black", method = "ellipse", type="upper", diag=FALSE, title= "Correlation Matrix: Number of Subscribers",mar=c(0,0,4,0))
#numSubscribers is positively correlated to  NumReviews, numPublishedLectures and duration



#################################
# Checking for Multicollinearity:
################################

model0 <- lm(log(numSubscribers+1) ~ numReviews+Category+log(numPublishedLectures+1) + duration + log(price+1)+isPaid+instructionalLevel, data=udemy)

# VIF for the above model considering every variable 
vif(model0) 
sqrt(vif(model0)) > 2  # No Multicollinearity Exits. 


###################################
# Regression Modeling and Analysis: 
###################################

options(scipen = 999)
#Model 1:
model1 <- lm(log(numSubscribers+1) ~ Category+log(numPublishedLectures+1), data=udemy)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
plot(hatvalues(model1))
identify(hatvalues(model1), col="red") # Identify the outliers and remove them. 

outliers1<- c( 9, 23, 136,  230,  271,  527, 1024, 1382, 2599, 2645, 2815, 2823, 2841, 3013, 3046, 3140, 3355, 3377, 3431, 3623, 3633, 3672)
udemy1 <- udemy[-outliers1,]
model1.1 <- lm(log(numSubscribers+1) ~ Category+log(numPublishedLectures+1), data=udemy1)
summary(model1.1)
par(mfrow=c(2,2))
plot(model1.1)

#Model 2:
model2 <- lm(log(numSubscribers+1) ~  log(price+1)+isPaid+Category+instructionalLevel, data=udemy)
summary(model2)
par(mfrow=c(2,2))
plot(model2)

plot(hatvalues(model2)) 
identify(hatvalues(model2), col="red")
outliers<- c(  188, 274,  394,  517,  566,  618,  800,  905,  912, 1138, 1176, 1388, 1455, 1695, 1761, 1776, 1919, 2035, 2246, 2316, 2493, 2565, 2685, 2716, 2829, 2866, 2978, 3054, 3080, 3280, 3282,3340, 3466, 3550, 3614)
udemy2 <- udemy[-outliers,]
model2.1 <- lm(log(numSubscribers+1) ~  log(price+1)+isPaid+Category+instructionalLevel, data=udemy2)
summary(model2.1)

par(mfrow=c(2,2))
plot(model2.1)

dev.off()
#################################################END########################################################





