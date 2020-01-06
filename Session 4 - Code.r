#=======================================
# IDS 462, Session 4
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#=======================================

# Load libraries
#= 
library(car)
library(corrplot)
library(gmodels)
library(psych)
library(tidyverse)
library(vcd) 

# Inspect the data 
#= 
set.seed(462)
bp %>% sample_n(200) %>% View() 

# Wrangle 
#= 
bp1 <- bp %>% select(permit="PERMIT_TYPE", date="ISSUE_DATE", est_cost="ESTIMATED_COST", 
	amnt_waived="AMOUNT_WAIVED", amnt_paid="AMOUNT_PAID", total_fee="TOTAL_FEE", 
	contractor_city="CONTRACTOR_1_CITY"
	) %>% drop_na() 

nrow(bp1)/nrow(bp)

gsub("PERMIT - ", "", bp1$permit) %>% head() 

bp1$permit <- gsub("PERMIT - ", "", bp1$permit) %>% as.factor() 

bp1$date <- lubridate::mdy(bp1$date)

numcols <- c("est_cost", "amnt_waived", "amnt_paid", "total_fee")

bp1[numcols] <- bp1[numcols] %>% mutate_all(funs(gsub("\\$","",.))) %>% mutate_all(funs(as.numeric)) # convert to numeric 

set.seed(462)
samp <- sample(1:nrow(bp), 20) 
bp1[samp,] %>% select(est_cost, amnt_paid, amnt_waived) 

bp1$contractor_city <- as.factor(bp1$contractor_city)

bp1 <- bp1 %>% select(PERMIT_TYPE, ISSUE_DATE, ESTIMATED_COST, AMOUNT_WAIVED, AMOUNT_PAID, CONTRACTOR_1_CITY) %>% glimpse()

# Descriptive univariate statistics 
#= 
summary(bp1) # Summarizes all the variables. This is a good starting point. 

# instead of scientific notation 
options(scipen=99) 
summary(bp1)
	# I don't like the 9999999999. How many do we have? 
	
bp1 %>% filter(est_cost==9999999999) %>% count() # got it

bp1 <- bp1 %>% filter(est_cost!=9999999999)  # got it

# Now for factor variables  
table(bp1$contractor_city) 

# Tidyverse 
bp1 %>% group_by(contractor_city) %>% count() 

bp1 %>% group_by(contractor_city) %>% count() %>% arrange(desc(n)) # too many levels (cities) 

bp1$contractor_city <- gsub("CHGO", "CHICAGO", bp1$contractor_city)

bp1$city <- ifelse(grepl("CHICAGO", bp1$contractor_city), "Chicago", "Other")

## Analysis 
#=
# Univariate 

# Factor variables 

tab_city <- table(bp1$city)

tab_permit <- table(bp1$permit)

prop.table(tab_city)

prop.table(tab_permit)

plot(tab_city)

barplot(tab_city)

barplot(prop.table(tab_city))

barplot(prop.table(tab_city), col=c("red", "orange"), 
	main="Distribution of Contractors by Location", 
	ylim=c(0,0.6))
	box(lwd=2) 

# Numeric variables

	
	# There are many functions and packages that provide excellent descriptive statistics for numeric variables, and we can use individual functions from base R, like mean(), and sd(). 

# Explore 

mean(bp1$est_cost)
median(bp1$est_cost)
sd(bp1$est_cost)
fivenum(bp1$est_cost)
quantile(bp1$est_cost, c(0.1, 0.3, 0.9))
IQR(bp1$est_cost)

# Tidyverse
bp1 %>% summarize(avg = mean(est_cost), median = median(est_cost), std = sd(est_cost)) 

# Psych is another great option 
desc<-describe(bp1[numcols])
class(desc)
View(round(desc,2))

# In addition to parameters, we should inspect distributions

# Histogram and density plots 

hist(bp1$est_cost)
hist(bp1$est_cost[bp1$est_cost<200000])
hist(bp1$est_cost[bp1$est_cost<50000])
hist(bp1$est_cost[bp1$est_cost<50000], breaks=50) # perhaps focus on <20k
hist(bp1$est_cost[bp1$est_cost<20000], main="Histogram of Estimated Cost (without extremes)") 
plot(density(bp1$est_cost))
plot(density(log(bp1$est_cost))) # Consider this transformation 

# overlaid hisogram and density plots 
est_cost_cut <- bp1$est_cost[bp1$est_cost<20000]
hist(est_cost_cut, prob=T, col="steelblue", main="Distribution of Estimated Cost (without extremes)") 
rug(est_cost_cut, col="gray", lwd=0.5)
lines(density(est_cost_cut), col="orange", lwd=3)

boxplot(bp1$est_cost) # not great  
boxplot(est_cost_cut, col="orange", main="Distribution of Estimated Cost (without extremes)") # better!

# qqplot plots the distribution of the variable against a normal distribution
qqnorm(bp1$est_cost) 
qqline(bp1$est_cost, col="red", lwd=2) # again... 
qqnorm(log(est_cost_cut+1))
qqline(log(est_cost_cut+1), col="red", lwd=2) # not surprising 

#################
# Examine the other numeric variables in the bp data. What did you find? 
# Examine a couple of numeric variables in "your" data. What did you find? 
#################

# Bivariate relationships 
#= 

# Two factors 

# Explore 
tab_permit_city <- table(bp1$permit, bp1$city)
ftab_permit_city <- ftable(bp1$permit, bp1$city) 
xtab_permit_city <- xtabs(~bp1$permit + bp1$city) 

tab_permit_city
ftab_permit_city # supports more than 2 factors 
xtab_permit_city # supports more than 2 factors + statistical tests 

p1_ftab_permit_city <- prop.table(ftab_permit_city, margin=1) # row % 
p2_ftab_permit_city <- prop.table(ftab_permit_city, margin=2) # column % 

addmargins(p1_ftab_permit_city) # can also be used without prop.table 
addmargins(p2_ftab_permit_city)

# Plot
barplot(prop.table(xtab_permit_city), col=1:10, ylim=c(0,1), main="Permits by City", beside=T) 
barplot(prop.table(xtab_permit_city), col=1:10, ylim=c(0,1), main="Permits by City") 
legend("toprigh", levels(bp1$permit), lty=1,lwd=4, col=1:10)
box(lwd=1.5) 

mosaic(xtab_permit_city, shade=T) # better when there are less levels 

# Test 
summary(xtab_permit_city) # or more directly 
chisq.test(bp1$permit, bp1$city) # but there's a better option
CrossTable(bp1$permit, bp1$city, prop.c = F, prop.r = F,
dnn = c('Permit', 'City'), format=c("SPSS"), chisq = T)

#################
# Examine the relationships among factor variables in "your" data. What did you find? 
#################

# Two numeric variables

# Plot 
plot(bp1$total_fee~bp1$est_cost, col="steelblue", pch=20, cex=0.75) # right, we need to address the outliers

bp2 <- bp1[bp1$est_cost<20000,]
plot(bp2$total_fee~bp2$est_cost, col="steelblue", pch=20, cex=0.75)
plot(log(bp2$total_fee+1)~log(bp2$est_cost+1), col="steelblue", pch=20, cex=0.75)

abline(lm(log(total_fee+1)~log(est_cost+1), data=bp2), col="red", lwd=2) # Not great 

# Correlation matrix

cormat <- cor(bp2[,numcols])
round(cormat,2)

corrplot(cormat, method="circle", addCoef.col="grey", type="upper") 

# pairs(bp1[,numcols]) # this one is slow. Try it later!  

# Test 

# Correlation 
cor(bp1$est_cost, bp1$total_fee)
cor.test(bp1$est_cost, bp1$total_fee) # As expected 


#################
# Examine the relationship between other numeric and variables in the bp data. What did you find? 
# Examine the relationships among numeric variables in "your" data. What did you find? 
#################

# Factor and a numeric variable 

# Explore 

aggregate(est_cost ~ city, data = bp1, FUN="mean", na.rm=T)
aggregate(est_cost ~ city, data = bp1, FUN="sd", na.rm=T)

# Tidyverse

bp1 %>% group_by(city) %>% summarize(avg=mean(est_cost), median=median(est_cost), sd=sd(est_cost)) 

# Plot

boxplot(est_cost ~ city, data=bp2, main="Comparing Distributions by City", 
	xlab="City", ylab="Estimated Cost", col=c("orange", "steelblue")) 

# Test

est_cost_model <- aov(log(est_cost+1)~city, data=bp2)
summary(est_cost_model) 

# Tukey pairwise comparisons
TukeyHSD(est_cost_model)
 
	
#################
# Examine the relationships among numeric and factor variables in the data. 
# Which variables seem to be highly correlated? 
#################


############### 
# Additional resources: 
# Learning R, ch 14
