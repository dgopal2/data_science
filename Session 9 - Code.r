#=========================================================
# IDS 462, Session 9 - OLS (cont.)
#=========================================================
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================

## LOAD LIBRARIES AND DATA 
#=
library(car) 
library(effects)
library(psych)

load("session9.Rdata") # retail, realestate 

# Regression assumptions (recap) 

# The DV is normally distributed
# IV values are independent
# There is a linear relationship between the DV and IVs
# There is constant variance, such that the variance of the DV 
	# does not change with the levels of IVs (homoscedasticity) 
# There is no multicollinearity (two or more IVs are highly correlated) 


# Outlier detection utility function 
outliers <- function(column) {
  lowerq <- as.vector(quantile(column)[2]) # returns 1st quartile
  upperq <- as.vector(quantile(column)[4]) # returns 1st quartile
  iqr <- upperq-lowerq  
  
# Moderate outliers
  
  mod.outliers.upper <- (iqr * 1.5) + upperq
  mod.outliers.lower <- lowerq - (iqr * 1.5)
  mod.outliers <- which(column > mod.outliers.upper | 
                column < mod.outliers.lower)
  print(paste("Moderate outlier:", mod.outliers))

  #Extreme outliers
  
  extreme.outliers.upper <- (iqr * 3) + upperq
  extreme.outliers.lower <- lowerq - (iqr * 3)
  extreme.outliers<-which(column > extreme.outliers.upper 
              | column < extreme.outliers.lower)
  print(paste("Extreme outlier:", extreme.outliers))
  
}


# Step 1: Make sure data are clean 
#= 
summary(retailer)
	# remove columns/rows, address NAs, convert types, if needed  #catalog has only 4 so it shud be a factor
#suspect in categorical values is few levels having too few observation or no values # shud remove
#here nothing needed


# Step 2: Examine at a univariate level (transform if needed) #we learnt log #few might not be useful after changing to log so gonna c other transformations
#= 
	# factors have reasonable levels and distribution 

	# let's examine the numeric variables 
View(describe(retailer[,c("income", "expense")])) #talk abt skew n spread #expense median 944 sd also similar so this has wider spread than first
plot(density(retailer$income)) # not perfect
plot(density(retailer$expense)) # skewed #since it had more sd has more skew as expectd

# outliers 
boxplot(retailer$income) # outlier #no outlier

# detect and remove outlier 
outliers(retailer$income) 
#retailer <- retailer[-101,] #initially had so removed but now no
 
boxplot(retailer$expense) # quite a few outliers 
outliers(retailer$expense) # only a few are extreme outliers #no extreme outlier now

retailer <- retailer[-c(48,193,448,497,603,675),]#extreme outliers r the first to go# only 50 rows dont want to remove even 5 moderate ouliers
#! instead of outliers can do transformations
# transformations 
plot(density(log(retailer$expense))) # works 
plot(density(sqrt(retailer$expense))) # better #wen we interpret results we ve to rem its in sqrt
#one unit of increase in sqrt of expense makes income increase by


plot(density(sqrt(retailer$income))) # nope 
plot(density(log(retailer$income))) # not perfect #at this level ok to wrk if its smwt normal
#log sqrt can explain back but complex to explain with power transfor but betr to examine

# When dealing with complex transformations try: 
summary(powerTransform(retailer$expense)) # interesting... let's compare #est power .25 power of the..taking sqrt twice p-value is 7.5e-9 so it means this transformation ll wrk
summary(powerTransform(retailer$income)) # confirms our choice! #power is .47 rounded power is .5 confirms sqrt n p-value also

par(mfrow=c(2,2))
plot(density(retailer$expense)) 
plot(density(log(retailer$expense)))
plot(density(sqrt(retailer$expense)))
plot(density(retailer$expense^0.23)) #this is power transformation power of .25 income power is 0.5 cuz sqrt
#do it fr income
par(mfrow=c(2,2))
plot(density(retailer$income)) 
plot(density(log(retailer$income)))
plot(density(sqrt(retailer$income)))
plot(density(retailer$income^0.5)) #if power transform too says .5 n gives gud p=value accept it #under time

# What if we have a "bad" numeric variable?  #expense power plot betr than sqrt cuz both end tails. can use sqrt too
retailer$catalog <- as.numeric(retailer$catalog)
cor.test(retailer$expense, retailer$catalog) # pretty good, but #!low p-value so there is correlation
plot(density(retailer$catalog)) # months would have been better #multimodal distribution #read it #one yr #2yr
	# Option 1: Try a power transformation (likely to fail)
	summary(powerTransform(retailer$catalog)) 
	plot(density(retailer$catalog^0.5)) # as expected #not very usful
	#! survey data too ll be so lik 5 data we ll be der ll get same plots
	#if transformations fail we ll try to build index #else try to turn it into fctr
	
	# Option 2: Make an index/scale
	# This option requires multiple comparable variables (high correlation, capture the same dimension) 
	# Very common in survey data. But we don't have such variables in the retailer data 
	
	# Option 3: Convert it into a factor 
	# First, check the distribution. If uneven, "bin" it to categories of about the same size 
	table(retailer$catalog) # looks fine 
	# Second, create a factor variable
	retailer$catalog <- as.factor(retailer$catalog)
	# Third, use the factor variable instead of the original variable in the model 
	#u got correlation n less p-value but dont rush it see the distribution step by step
	
# Final thoughts on transformations: 
	# Try the simplest forms, i.e., log, sqrt
	# Try creating an index that consists of multiple highly related variables
	# Try creating a factor out of "bad" variables
	# Ultimately, we balance meeting modeling assumptions with interpretation. Avoid overly complex transformations! 

#################
# Examine the realestate data for univariate numeric transformations
# If needed, create new transformed variable(s) 
#################
#my ans:
	par(mfrow=c(2,2))
	plot(density(realestate$lotsize))
	plot(density(log(realestate$lotsize)))
	plot(density(sqrt(realestate$lotsize)))
	plot(density(realestate$lotsize^-0.21))
	
	dev.off()
	#log looks betrfr lotsize
	
	summary(powerTransform(realestate$lotsize)) 
	summary(powerTransform(realestate$price)) 
	par(mfrow=c(2,2))
	plot(density(realestate$price))
	plot(density(log(realestate$price)))
	plot(density(sqrt(realestate$price)))
	plot(density(realestate$price^-0.23))
#log or power trans both looks gud
	
	#proper way i guess
	
	boxplot(realestate$lotsize) # outlier 
	
	# detect and remove outlier 
	outliers(realestate$lotsize) 
	realestate <- realestate[-c(365,369,359,434,446,365),] #removing those rows which has outliers
	
	
	boxplot(realestate$price) # outlier #no outlier
	
	# detect and remove outlier 
	outliers(realestate$price)
	realestate <- realestate[-c(332,362,375,416,93,94,416,433),]
	#after this do the plots
	summary(powerTransform(realestate$lotsize)) #pvalue is .15 so dont use it. devansh says shudn use rounded
	summary(powerTransform(realestate$price))#pvalue not good
	
	par(mfrow=c(2,2))
	plot(density(realestate$lotsize))
	plot(density(log(realestate$lotsize)))
	plot(density(sqrt(realestate$lotsize)))
	#log looks betr
	
	dev.off()
	#log looks betrfr lotsize
	
	
	par(mfrow=c(2,2))
	plot(density(realestate$price))
	plot(density(log(realestate$price)))
	plot(density(sqrt(realestate$price)))
	#log looks betr
	dev.off()
	
# Step 3: Examine bivariate relations  (transform if needed)
#= 
# Which IVs do we need to included
# Explore correlation between DV and IVs, and among IVs
# The idea is to have a simple model that explains the DV, but not too simple 

# Before transformation 
dev.off() 
plot(retailer$expense~retailer$income, pch=16, col="darkgrey")#scatterplot#can c positive linear reln with few exceptions
cor.test(retailer$expense,retailer$income)#getting high correlation
plot(sqrt(retailer$expense)~retailer$income, pch=16, col="darkgrey")#even betr positive

# A correlation plot would be useful, but we only have two numeric variables in the data

# Let's explore other relationships in the data 

# Now DV and IVs 
summary(aov(expense ~ agegroup, data=retailer)) #***
summary(aov(expense ~ gender, data=retailer)) #***
summary(aov(expense ~ homeownership, data=retailer)) #***
summary(aov(expense ~ maritalstatus, data=retailer)) #***
summary(aov(expense ~ location, data=retailer)) #***
#all r stat significant #so they gonna have a effect onDV so shud add them to analysis
#ve to determine reln among them to c collinearity

# Also consider relationships among IVs 
chisq.test(retailer$catalog, retailer$agegroup) #***
chisq.test(retailer$catalog, retailer$gender) # (NS) 
chisq.test(retailer$catalog, retailer$homeownership) #***
chisq.test(retailer$catalog, retailer$maritalstatus) #. 
chisq.test(retailer$catalog, retailer$location) #.
chisq.test(retailer$agegroup, retailer$gender) #***
chisq.test(retailer$agegroup, retailer$homeownership) #***
chisq.test(retailer$agegroup, retailer$maritalstatus) #***
chisq.test(retailer$agegroup, retailer$location) #*
chisq.test(retailer$agegroup, retailer$gender) #***
# you've got the gist 
# we need to pay attention to these relationships! 

###################
# Perform steps 1-3 for predicting house prices
# Note that we have already done much of this work in the past
###################

# Step 4: modeling 
#= 

mod1 <- lm(expense~income+agegroup+gender+homeownership+maritalstatus+location, data=retailer)
#usualyy we wont start by adding all variable #since all add stat sig added them
mod1 <- lm(sqrt(expense)~income+agegroup+gender+homeownership+maritalstatus+location, data=retailer)
#sqrt#first say signn or not n postive or negative then #for every one unit of increase of income it increases sqrt of one unit of expense
#the diff betn old n young ppl  stat sig #the diff betn middle n young r not sig
#wen comparing factor see with other category not with DV # ppl out of state vs in state
#compared to female
#started with correlation so all had so included #plots too confirmed
#but after doing test will include only stat signifcantS

# Interpreting the results 
summary(mod1)

# coefficients 
summary(mod1)$coefficients[,1]

# dummies 
	# if we want another reference category
retailer$agegroup<-relevel(retailer$agegroup, ref=3)#usually r takes refernce which has low alphanumeric so female n not male #here fr young changing

#change ref=2 n c if it changes d defult refernce
mod2 <- lm(expense~income+agegroup+gender+homeownership+maritalstatus+location, data=retailer)
summary(mod2)

# Notice the R and Adjusted R-squared 
summary(mod1)$r.squared 
summary(mod1)$adj.r.squared # "penalty" for adding more variables #increase of 5% but shud lookat adj rsq

# Not bad, but we need to do some more work to fine-tune our model. 

###################
# Build a model to predict house prices
# Find a model that has the best adj-r-squared
# Know how to interpret the results!
###################
#my ans


summary(aov(price ~ lotsize, data=realestate)) #shud be regrn
summary(aov(price ~ bedrooms, data=realestate)) 
summary(aov(price ~ bathrooms, data=realestate))
summary(aov(price ~ stories, data=realestate))
summary(aov(price ~ driveway, data=realestate))
summary(aov(price ~ recroom, data=realestate))
summary(aov(price ~ fullbase, data=realestate))
summary(aov(price ~ gashw, data=realestate))
summary(aov(price ~ airco, data=realestate))
summary(aov(price ~ garagepl, data=realestate))
summary(aov(price ~ prefarea, data=realestate))

moda <- lm(price~lotsize+bedrooms+bathrms+stories+driveway+recroom+fullbase+gashw+airco+garagepl+prefarea, data=realestate)
moda <- lm(log(price)~lotsize+bedrooms+bathrms+stories+driveway+recroom+fullbase+gashw+airco+garagepl+prefarea, data=realestate)
moda <- lm(sqrt(price)~lotsize+bedrooms+bathrms+stories+driveway+recroom+fullbase+gashw+airco+garagepl+prefarea, data=realestate)

summary(moda)
summary(moda)$r.squared 
summary(moda)$adj.r.squared
# coefficients 
summary(moda)$coefficients[,1]


# sir ans

#plot density price skwd
#so takes plot log #fr bathrms he wants to c if it is factor#convert #mod log cuz log helped #
#table (as.factor(real$bath)) #u see 4 bthm - 1 3-10 #dont skip any step
#1bthrm missing here is d ref uz 1 loe alpha #all stat sig #

# Interactions 
#= 
# Used when we suspect that the effect of a predictor might vary at different values (levels)
# This means, that the relationships between an IV and the DV is dependent on the value of another IV

mod2_terms<-lm(log(price)~lotsize+bedrooms+airco+lotsize:prefarea, data=realestate) # adding an interaction term of lotsize and location
#we taking log cuz we saw tats d crct approach
#prefarea want to know interactionso including it with all other terms
#intercation is stat sig

summary(mod2_terms) # Something is certainly going on here 
library(effects)
#compares slopes of diff area #if there i effect there is interaction
plot(effect(term="lotsize:prefarea", mod=mod2_terms)) #yes no diff so putting in one plot
plot(effect(term="lotsize:prefarea", mod=mod2_terms, default.levels=20), multiline=T) # same plot #sig diff we c now in plot
#if we c both same on top of other no interaction



# You can try ^n interactions, but we try to avoid them unless they add considerable contribution to the model. We do it this way
mod2_n_terms<-lm(log(price)~lotsize+bedrooms+airco+bathrms*bedrooms*lotsize*prefarea, data=realestate) 
summary(mod2_n_terms) # Indeed, not very useful. #result - none of them sign #decision tree splits based on interaction
 
#################
# What other interactions exist in the realestate data? 
#################
#my ans
str(realestate)
realestate$bedrooms <- as.factor(realestate$bedrooms)
realestate$bathrms <- as.factor(realestate$bathrms)
mod3_terms<-lm(log(price)~lotsize+bedrooms+airco+bedrooms:bathrms, data=realestate)

summary(mod3_terms)
#bedrooms:bathrms 5.743e-02  7.623e-03   7.534 2.44e-13 *** # so there is interaction effect


plot(effect(term="bedrooms:bathrms", mod=mod3_terms)) #yes no diff so putting in one plot
plot(effect(term="bedrooms:bathrms", mod=mod3_terms, default.levels=20), multiline=T) #jumps quickly from 5-6 pink
#green from 4-5 drops #bedrooms has effect on rpice but its mitigated by interaction it has with bathrms #guess this is wat he said

#gotta do the table 
table(realestate$bedrooms) #remove 5 6
table(realestate$bathrms) #remove 
#there is interaction betn 2 in terms of preidtcing the price


# Diagnostics 
#=
plot(mod2) # This is a great way to identify multiple possible concerns with the model 
# For example, outliers. #this gives outlier in bivariate #gotta remove #in all 4 plots the outliers reapats we gotta remove them
# Better yet: 
par(mfrow=c(2,2)) # This function sets up the plotting canvas to 2x2, so that we don't need to go through them one by one. Now again:  
plot(mod2)
# We could also plot diagnostics "manually", such as: 
dev.off() 
plot(predict (mod2), residuals (mod2))
plot(hatvalues(mod2)) 
identify(hatvalues(mod2), col="steelblue") # Neat, but this interactive feature doesn't always work, so: 
tail(sort(hatvalues(mod2)), n=5)

# Cook's distance indicates other "suspects" as well. We need to examine them and decide what to do with these cases
realestate[c(103,185,414,491,365,369,383,366), c("price", "lotsize", "bedrooms", "airco", "prefarea")]

# We could remove these observations, and re-run our model 
outliers <- c(103,185,414,491,365,369,383,366)
realestate_new <- realestate[-outliers,]
nrow(realestate_new)-nrow(realestate) # We removed 8 
mod3<-lm(log(price)~lotsize+bedrooms+airco+lotsize:prefarea, data=realestate_new) 

# Did our model improve? 
# We want to know to find the extent to which our models have improved, after "cleaning"
summary(mod2)$adj.r.squared-summary(mod1)$adj.r.squared  
summary(mod3)$adj.r.squared-summary(mod2)$adj.r.squared  

#################
# Add other variables to the model. Which outliers (observations/rows) did you find? What is the best adjusted R-squared that you got? 
#################


## ADDITIONAL RESOURCES
#=
# Learning R, Ch. 15
# OpenIntro Statistics. Ch. 7
# R in Action (2nd. Edition), Ch. 8