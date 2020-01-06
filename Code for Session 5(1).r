#=========================================================
# IDS 462, Session 5 
# Data Visualization
#=========================================================
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

## (INSTALL) LOAD LIBRARIES 
#=
library(ggplot2) # already intalled
library(gridExtra)
library(plotly)
library(RColorBrewer)
library(vcd) # already intalled

## LOAD DATA 
load("Session 5.RData")

## BACKGROUND 
#= 
# graphics is what base R uses 
# developed later, grid is the foundation for both lattice and ggplot2 
# lattice is now a part of R
# ggplot2 is a separate package 

# R base
#= Plotting parameters 

# Univariate plots 
#== 
#plot_par <- par()
# Factor 
barplot(table(cars$transmission))
barplot(table(cars$transmission), ylim=c(0,140), 
        axes=F, #to remove axes
        col=terrain.colors(2),
        cex.main=1.5, 
        col.main="darkgray", 
        main="Cars by Transmission Type")
axis(side = 2, at = seq(from=0, to=140, by=20))# zoom in!
box(lwd=5)
#pdf("barplot.pdf")
#########
# Generate a bar plot for driveway 
# Use different colors for having/not-having a driveway 
# Title the plot
# Encapsulate in a box 
#########
#ans:
barplot(table(realestate$driveway))
barplot(table(realestate$driveway), ylim=c(0,500), 
        axes=F, #to remove axes
        col=terrain.colors(3),
        cex.main=1.5, 
        col.main="darkgray", 
        main="Real Estate by Driveway")
axis(side = 2, at = seq(from=0, to=500, by=20))# zoom in!
box(lwd=3)
pdf("barplot.pdf")
dev.off()


# Numeric variable 
# Histogram 
hist(baseball$pay, 
     col=rainbow(10), 
     breaks=10, 
     main="Histogram of budget")

# detour: advanced color option 
	display.brewer.all()
	brewer.pal.info

# apply a color theme 	
	hist(baseball$pay, 
	     col=brewer.pal(10, "Set3"), 
	     breaks=10, 
	     main="Histogram of budget")

# change lable names 
hist(baseball$pay, 
     col=brewer.pal(10, "Set3"), #set3, paird choosing one pallette #if 10 choosing 10 colors. if choose 2 here ll keep circlng i.e. repeat
     breaks=10, 
     xlab="Budget in million $",
     ylab="Number of teams",
     main="Histogram of budget")
box(lwd=2) # lwd is line width 

# Histogram with overlaid density and rug plots 
hist(baseball$pay, 
     freq=F,
     col=brewer.pal(10, "Dark2"), 
     breaks=10, 
     xlab="Budget in million $",
     main="Histogram of budget")

rug(jitter(baseball$pay), col="darkgray")
lines(density(baseball$pay), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
box(lwd=2)

# Boxplot 
boxplot(baseball$pay, 
        col="coral", 
        main="Boxplot of budget")

# Side-by-side 
par(mfrow=c(1,2)) # par is for plotting parameters; mfrow is for number of rows/columns
hist(baseball$pay, col=c("steelblue", "red"), freq=F, xlab="budget in Millions", main="Distribution of budget") 
rug(jitter(baseball$pay), col="darkgray")
lines(density(baseball$pay), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
box(lwd=1.5)
boxplot(baseball$pay, col="orange", main="Boxplot of budget") #imp for exam

dev.off() # closes and resets the plotting canvas

# Saving output 
pdf("dist_budget.pdf", height=7, width=8)
par(mfrow=c(1,2)) #think 1 row 2 column chnag 2,1 # exam need to giv verything lik this
hist(baseball$pay, col=c("steelblue", "red"), freq=F, xlab="budget in Millions", main="Distribution of budget") 
rug(jitter(baseball$pay), col="darkgray")
lines(density(baseball$pay), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
boxplot(baseball$pay, col="orange", main="Boxplot of budget")
box(lwd=1.5)
dev.off() # closes the pdf file 

dev.off() # resets the plotting canvas 

#########
# Generate plots for the distribution of realestate lotsize
# and the distribution of bedroorms (as a factor)
# x and y axes should be labeled properly, 
# and the plots should have a title
# Save the plots side-by-side as a PDF file 
#########
#solution:par(mfrow=c(1,2)) # par is for plotting parameters; mfrow is for number of rows/columns
hist(realestate$lotsize, col=c("steelblue", "red"), freq=F, xlab="budget in Millions", main="Distribution of lotsize") 
rug(jitter(realestate$lotsize), col="darkgray")
lines(density(realestate$lotsize), col="yellow", lwd=3) # Note how the lines function is used to overlay the density plot 
box(lwd=1.5)
boxplot(realestate$bedrooms, col="orange", main="Boxplot of budget")
# Bivariate plots 
#== 

# Two numeric variables 
#- 
plot(baseball$pct~baseball$pay) 

# Improved version 
plot(baseball$pct~baseball$pay, data=baseball, 
main="Relationship between budget and PCT", 
ylab="% wins", 
xlab="budget in $million", 
pch=20, 
col="orange",
xlim=c(35, 200)) 
abline(lm(baseball$pct~baseball$pay), col="blue", lwd=2, lty=2) #linetype
lines(loess.smooth(baseball$pay,baseball$pct), col="red", lwd=2, lty=1)#shows we have smwth of a linear reln#regrn assumption is normally distributed but this is not so trying to smooth it out
# add identifiers (label observations)
text(baseball$pct~baseball$pay, cex=.6, col="steelblue", labels=baseball$team)
ticks <- c(35, 70, 100, 150, 200)
axis(side=1,at=ticks) # sides are clockwise from bottom=1 to right=4
abline(h=0.5, v=ticks, lty=2, col="gray") # also may use grid to add gridlines 
box(lwd=2.5, lty=3)  #wont ask in exam few things few might

#########
# Plots the relationship of price and lotsize
# Add colors, and change tick marks 
# Add a regression line with a different color 
# x and y axes should be labeled properly, and the plot should have a title
# Save the plots as a PDF file 
#########

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

# Numeric variable and a factor 
boxplot(pay~division, data=baseball, 
        col=brewer.pal(3, "RdGy"), 
        main="Distribution of budget\nby divisions") # \n is a new line 

#########
# Plot the relationship of realestate price and lotsize, 
# by prefarea
# Add a regression line 
# Add a legend 
#########

	
## PLOTTING WITH GGPLOT2  
#=

# Today, this is the most popular library for plotting in R (and increasingly in Python)
# It can get a bit complicated, because there are many options
# It is based on the  grammar of graphics, similar to 
# the notion of verbs (like dplyr, tidyr, and other packages in tidyverse) 


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

# Univariate
#- 
ggplot(data=baseball, aes(x=pay)) + 
geom_histogram(fill="steelblue", color="gray", alpha=0.8, bins=10) + 
geom_rug(aes(x = pay, y = 0))

ggplot(data=baseball, aes(x=pay)) + 
  geom_density() 

# While overlaying histogram and density plot 
# is possible with ggplot, it is overly complicated. 

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

ggplot(data=re) + aes(x=airco, fill=bedrooms) + 
	geom_bar(position="fill")+
	scale_fill_brewer(palette="Pastel2") + 
  labs(title='Relationship between Bedrooms and AC',
       x="AC", y="Frequency") 

# Numeric and factor  
baseball %>% ggplot(aes(x=pay, fill=division)) + 
  geom_density(alpha=.3) # comparative density plot 

# Three or more variables 

# Facets are useful because they allow breaking up plots by groups. 
ggplot(data=cars, aes(x=price)) + 
     geom_histogram(bins=10) + 
     facet_wrap(~transmission)

ggplot(data=cars, aes(x=mileage, y=price, 
    color=transmission)) + 
     geom_point() + 
    facet_grid(.~model) # grid used for two variables, wrap for one

# Two numeric and two factors! 
ggplot(baseball, aes(x=wins, y=pay, 
    color=division, 
    shape=league)) +
    geom_point() + geom_jitter() 
	   
# Statistical plots (e.g., linear and smoothers)
#-
p1 <- ggplot(data=realestate, aes(x=log(lotsize),y=log(price))) +
       geom_point()  # it is possible, and advisable, to assign ggplot output into an object 
   
p1 + geom_smooth(method="loess")  + 
    geom_smooth(method="lm") # se = F will remove the CI band 

# Arranging ggplots on canvas 
#- 
par(mfrow=c(1,2))
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

# Interactive Plots 
#=
p1 <- plot_ly(data = cars, x = ~mileage, y = ~price)  
p1 

# or better 
p1.1 <- plot_ly(data = cars, x = ~mileage, y = ~price, 
                type="scatter", mode="markers" , 
                marker=list(color="steelblue" , size=10 , opacity=0.5))
p1.1

# heatmap (two factors with many levels)
p2 <- plot_ly(data = cars, x = ~color, 
    y = ~as.factor(year))
p2

## ADDITIONAL RESOURCES
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