#=========================================================
# IDS 462, Session 6 
# Data Visualization, OLS
#=========================================================
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

library(corrplot) 
library(gridExtra)
library(tidyverse)
library(plotly) #tableau related
library(RColorBrewer)

load("session6.Rdata") # baseball, cars, realestate 

## Viz. continued .... 


# Two factors 
table(realestate$bedrooms, realestate$airco) # too few observations

re <- realestate %>% filter(bedrooms<5 & bedrooms >1)

re$bedrooms<-as.factor(re$bedrooms)
re$bathrms<-as.factor(re$bathrms)

tab<-table(re$bedrooms,re$airco)
proptab<-prop.table(tab)

barplot(proptab, main="Air Conditioning by Bedrooms",
     names.arg=c("No", "Yes"),col=brewer.pal(3, "Set2"), legend=c("2 bdr","3 bdr","4 bdr"), ylim=c(0,1.1))
box(lwd=2, col="steelblue")

#########
# Plots the relationship of airco and recroom 
# Add colors 
# Title the plot 
# Add a legend 
# Save the plots as a PDF file 
#########
#my ans:
table(realestate$recroom, realestate$airco) # too few observations


realestate$recroom<-as.factor(realestate$recroom)
realestate$airco<-as.factor(realestate$airco)

tab<-table(realestate$recroom,realestate$airco)
proptab<-prop.table(tab)
proptab

barplot(proptab, main="Air Conditioning by recrooms",
        names.arg=c("No", "Yes"),col=brewer.pal(3, "Set2"), legend=c("yesr","nor"), ylim=c(0,1.1))
box(lwd=2, col="steelblue")
chisq.test(realestate$recroom, realestate$airco)


# Numeric variable and a factor 
boxplot(pay~division, data=baseball, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of budget\nby divisions") # \n is a new line 

# A comparative kernel density plot 
plot(density(baseball$pay[baseball$division=="Central"]), col="red", lwd=2, ylim=c(0,0.02))
lines(density(baseball$pay[baseball$division=="East"]), col="blue", lwd=2)
lines(density(baseball$pay[baseball$division=="West"]), col="green", lwd=2)

#########
# Plot the relationship of realestate lotsize 
# and prefarea
#########
boxplot(lotsize~prefarea, data=realestate, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of lotsize\nby prefarea") # \n is a new line 

# A comparative kernel density plot 
plot(density(realestate$lotsize[realestate$prefarea=="Yes"]), col="red", lwd=2, ylim=c(0,0.02))
lines(density(realestate$lotsize[realestate$prefarea=="No"]), col="blue", lwd=2)
## PLOTTING WITH GGPLOT2  
#=

# Today, this is the most popular library for plotting in R (and increasingly in Python)
# It can get a bit complicated, because there are many options
# It is based on the  grammar of graphics, similar to 
# the notion of verbs (like dplyr, tidyr, and other packages in tidyverse) 

#gg means grammer of graphics
# Uses the following grammar: 
# data - must be mapped to aesthetic attributes 
# aesthetics - ways to represent data 
# geometry - describe plot type (bars, lines, points...)
# statistics - smoothers, bins...
# scale - associate spaces and actual data values 
# coordinates - coordinate system on which data are displayed
# faceting - grouping subsets of data 

# qplot = simple version, but less options 
#=

# Example 
#-

# Plotting three variables (two numeric and a factor)
ggplot(data=baseball, # data 
	aes(x=pay, y=pct, color=division)) + # aesthetic = variables 
	geom_point(pch=20, size=4) + # geometry = plot type 
	#stat_smooth(method="lm", se=T, linetype=1, lwd=1.5) + # added statistical plot (default is with CI, se=T)
	labs(title="Relationship between PCT and Pay by League"
	     , x="Pay", y="%wins") # annotation 

# But let's start with the basics 

# Univariate
#- 

# numeric 
ggplot(data=baseball, aes(x=pay)) + 
  geom_density() 

ggplot(data=baseball, aes(x=pay)) + 
geom_histogram(fill="steelblue", color="gray", alpha=0.8, bins=10) + 
geom_rug(aes(x = pay, y = 0))

# factor 
ggplot(data=baseball, aes(x=division))  


# While overlaying histogram and density plot 
# is possible with ggplot, it is overly complicated. 
# Likewise, generating a boxplot for a single variable. 

# Bivariate 
#= 

# Two numeric 
ggplot(data=baseball) + aes(x=pay, y=pct) +
	geom_point(pch=16, color="coral") +
  labs(title='Relationship between PCT and Pay',
       x="Pay", y="PCT") + # x for xlab, y for ylab 
  geom_smooth(method="lm", color="black", lwd=2)

# Two factors  
ggplot(data=re) + aes(x=airco, fill=bedrooms) + 
	geom_bar(position="stack")+
	scale_fill_brewer(palette="Pastel2") + 
  labs(title='Relationship between Bedrooms and AC',
       x="AC", y="Frequency") 

ggplot(data=re) + aes(x=airco, fill=bedrooms) + 
	geom_bar(position="dodge")+
	scale_fill_brewer(palette="Pastel2") + 
  labs(title='Relationship between Bedrooms and AC',
       x="AC", y="Frequency") 
#this stretches the plot to 100% - there r 2 options above.. i orefer this use this in exam.. practise a lot of this
ggplot(data=re) + aes(x=airco, fill=bedrooms) + 
	geom_bar(position="fill")+
	scale_fill_brewer(palette="Pastel2") + 
  labs(title='Relationship between Bedrooms and AC',
       x="AC", y="Frequency") 

# Numeric and factor  
baseball %>% ggplot(aes(x=division, y=pay)) + 
  geom_boxplot(aes(fill = division)) + 
  scale_fill_brewer(palette="Set2")
# foll is density kernel plot 
baseball %>% ggplot(aes(x=pay, fill=division)) + 
  scale_fill_brewer(palette="Spectral")+
  geom_density(alpha=.5) #alpha capcity of plot
  

# Three (or more) variables 

# Facets are useful because they allow breaking up plots by groups. 
ggplot(data=cars, aes(x=price)) + 
     geom_histogram(bins=10) + 
      facet_wrap(~transmission) #if u want his side by side split canvas mfrow kinda

ggplot(data=cars, aes(x=mileage, y=price, 
    color=transmission)) + 
     geom_point() + 
    facet_grid(.~model) #4factors #price vs mileage by tansmission by model #facets always #no much detail
# grid used for two variables, wrap for one

# Two numeric and two factors! #!another way yto do it
ggplot(baseball, aes(x=wins, y=pay, 
    color=division, 
    shape=league)) +
    geom_point() + geom_jitter() #both difficukt to interpret
	   
# Statistical plots (e.g., linear and smoothers)
#-
p1 <- ggplot(data=realestate, aes(x=log(lotsize),y=log(price))) +
       geom_point()  # it is possible, and advisable, to assign ggplot output into an object 
   
p1 + geom_smooth(method="loess")  + 
    geom_smooth(method="lm") # se = F will remove the CI band #!not a perfect linear reln

# Arranging ggplots on canvas 
#- 
par(mfrow=c(1,2))#multple ggplot on same canvas #in r base use mfrow#lil tricky with ggplot
ggplot(data=baseball, aes(x=pay)) + geom_histogram() 
ggplot(baseball, aes(x=pay, y=pct, color=league, shape=division)) +
  geom_point() + geom_jitter() # nope 

p1 <- ggplot(data=baseball, aes(x=pay)) + geom_histogram() 

p2 <- ggplot(baseball, aes(x=pay, y=pct, color=league, shape=division)) +
  geom_point() + geom_jitter() 
  
grid.arrange(p1, p2, ncol=2)

# Saving ggplots 
#- 
ggsave(file="budgethist.png", plot=p2, width=5, height=5.5) # width and height are in inches 
# As in R base you can also save as .pdf and many other formats 

#########
# Using ggplot
# Plot the relationship of lotsize and price, 
# by prefarea 
# explain your findings  
#########

ans:
  
  ggplot(data=realestate) + aes(x=lotsize, y=price) +
  geom_point(pch=16, color="coral") +
  labs(title='Relationship between LotSize and Price',
       x="LotSize", y="Price") + # x for xlab, y for ylab 
  geom_smooth(method="lm", color="black", lwd=2)
#shud use 2numeric one factor 2 scatterplot thing or use facet
# Interactive Plots 
#=
p1 <- plot_ly(data = cars, x = ~mileage, y = ~price)  
p1 #too many bins

# or better 
p1.1 <- plot_ly(data = cars, x = ~mileage, y = ~price, 
                type="scatter", mode="markers" , 
                marker=list(color="steelblue" , size=10 , opacity=0.5))
p1.1 #moving pointer says things

# heatmap (two factors with many levels)
p2 <- plot_ly(data = cars, x = ~color, 
    y = ~as.factor(year))
p2

### OLS #least sq method#to predict given a dependent variable

# Regression assumptions 

# The DV is normally distributed
# IV values are independent
# There is a linear relationship between the DV and IVs
# There is constant variance, such that the variance of the DV 
# does not change with the levels of IVs (homoscedasticity) 
# There is no multicollinearity (two or more IVs are highly correlated) 


## Simple linear regression 
#============================
# Our DV is price
# Let's correlate numeric vars (in this case only the DV and two IVs: mileage, year)
carsnum <- cars[,c(1,3,4)]
class(carsnum)
cormat <- cor(carsnum)
corrplot(cormat, addCoef.col = "gray")

# Both IVs exhibit a relationship 

# Now we can model (regress) price given mileage #we seee correlan betn proce yr mileage#including in model
mod1<-lm(price ~ mileage, data=cars)
mod1
options(scipen=99)
summary(mod1) #adj rsq. by knowing mileage we can predict price with 65% accuracy
confint(mod1) #interpret it as fr a unit incease of mile per unit wats the price range

# What about price given year? 
mod2<-lm(price ~ year, data=cars)
mod2
summary(mod2) 
# we can also set confidence interval at 99% 
confint(mod2, level=0.99)

# Predict specific values based on a model

# For example, the predicted price of a car with 5,10,100k miles: 
predict(mod1 , data.frame(mileage =(c(5000 ,10000 ,100000) )),
        interval ="confidence", level=0.95) 

# For example, the predicted price of a car from years 2002,2005,2008: 
predict(mod2 , data.frame(year =(c(2002 ,2005 ,2008) )), 
        interval ="confidence", level=0.95) # 1 2 3 is years

# How well did our model perform? 
head(cars$price)-head(predict(mod1)) # or using this function 
summary(residuals(mod1))
boxplot(residuals(mod1))
plot(cars$price~cars$mileage, pch=16, col="lightblue")
abline(mod1, col="red", lwd=3) # and this is what we've been plotting all along! 

# R-sqrd
summary(mod1)$adj.r.squared # The simple (single IV) model explains about 65% of the variance 

########################
# Build a regression model using lotsize as the regressor 
# What is the predicted price for lot 500, 1000, 2000, at 99% confidence level? 
# Find the adjusted R-sqrd 
########################
# What about price given year? 
mod2<-lm(price ~ lotsize, data=realestate)
mod2
summary(mod2) 
# we can also set confidence interval at 99% 
confint(mod2, level=0.99)

# Predict specific values based on a model

# For example, the predicted price of a car with 5,10,100k miles: 


# For example, the predicted price of a car from years 2002,2005,2008: 
predict(mod2 , data.frame(year =(c(500 ,1000 ,2000) )), 
        interval ="confidence", level=0.95)

# Diagnostics (residuals and influence/leverage) #nt fr exam
#=
par(mfrow=c(2,2)) # sets up the canvas to 4 plots, so that we don't need to go through them one by one 
plot(mod1)

# or 
dev.off()
plot(predict (mod1), residuals (mod1)) # we are looking for a "no pattern"/non-linearity 
# finding outliers can be done this way as well
plot(hatvalues(mod1)) 
identify(hatvalues(mod1), col="red")
# or because it looks like we have 2 outliers, we can do this 
tail(sort(hatvalues(mod1)), n=2)

outliers <- c(90, 149, 1)
cars1<-cars[-outliers,]
mod1.1<-lm(price ~ mileage, data=cars1) 
summary(mod1.1); summary(mod1) # some improvement 

########################
# Are there any outliers in the model you built? 
# Regress without 5 most "extreme" outliers 
# Did model performance change? 
# Now try to model price using a log transformation of price and lotsize
# Which model do you think is better (logged or non-logged)? Why? 
# Perform regression diagnostics to the logged model. 
# Finally, run the above (logged) regression without outliers. 
# Explain your findings. 
########################


## ADDITIONAL RESOURCES 

# DATA VISUALIZATION 
#=
# Grahpics in base R and packages: 
# run demo(graphics) 
# graphics parameters: http://www.statmethods.net/advgraphs/parameters.html 
# https://cran.r-project.org/doc/contrib/Short-refcard.pdf
# Gohil, A. 2015. R Data Visualization Cookbook. 

# ggplot2: 
# Wickham, H. 2016. ggplot2: Elegant Graphics for Data Analysis, 2nd edition. 
# Comprehensive resource for ggplot2: http://ggplot2.org/book/
# More up to date references: http://docs.ggplot2.org/current/index.html
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

# Additional cool plots (with code): 
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Scatterplot


# OLS
#=
# OpenIntro Book, ch. 7
# Learning R Book, ch. 15 (Linear Regressions) 