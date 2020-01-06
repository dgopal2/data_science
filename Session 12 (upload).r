#========================================================================= 
# IDS 462, Session 12  
#======================================================================== 
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission 
# from the instructor.  
#======================================================================== 

## LOAD DATA AND LIBRARIES  
#=
library(car)
library(corrplot)

library(rms)# for pseudo-R-Squared
library(psych)
library(tidyverse)

load("session12data.Rdata")

## Common issues and tips
## Logit (cont.) 
## Exploratory factor analysis 
## Common issues and tips


## Common issues and tips
#=
# The problem: identify and remove outliers 

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
  print(paste(mod.outliers))
  
  #Extreme outliers
  
  extreme.outliers.upper <- (iqr * 3) + upperq
  extreme.outliers.lower <- lowerq - (iqr * 3)
  extreme.outliers<-which(column > extreme.outliers.upper 
                          | column < extreme.outliers.lower)
  print(paste(extreme.outliers))
  
}

# Either compute +/-1.5*IQR, or use the utility function I provided (outliers)
# If you use the outlier function, you can find the outlier rows, but you can't directly use 
# to remove these rows. So, 

# How many outliers are in the data? 
num_outliers <- outliers(bank$balance) %>% length()
num_outliers/nrow(bank) # around 5%, so let's drop them 

outliersvec <- outliers(bank$balance) %>% as.numeric # generate a numeric vector of outliers#outliers shows with quotes cant subset with characters so ocnvert as  number 
bank1 <- bank[-outliersvec,] # bank1 is without outliers in balance 

plot(density(bank$balance)) # bad
plot(density(bank1$balance)) # not too bad 

# The problem: can you start with a regression model then perform EDA? 

# Not recommnded. Remember the debt variable from the bank model? What about poutcome? 
bank_mod3 <- glm(y~balance+marital+loan+poutcome, data=bank1, family=binomial)
summary(bank_mod3)
# Wow! Look at the z-value for poutcome sucess. Unkown is significant as well.#z value gves magnitude more value  

#But wait, how many cases are there?
table(bank1$poutcome) #gud fr prediction but not fr analysis


# So, how meaningful and useful is this variable? 
# Useful for prediction, but practial application is likely limited. 


# The problem: how to recode a variable with many levels? #ifelse, gsub, with subsetting

# Remember that you can recode a variable with an ifelse function. For example, 
bank1$marital_binary <- ifelse(bank1$marital=="married", "married", "not married") 

table(bank1$marital_binary)

# But if we have more levels it becomes complicated (a lot of nested ifelse statements), or clunky assignments. 

# The car package has what I think is the best option, with the recode function. Here's how it works: 
bank1$season <- car::recode(bank1$month, "c('sep', 'oct', 'nov')='fall'; 
                            c('dec', 'jan', 'feb')='winter'; 
                            c('mar', 'apr', 'may')='spring';
                            c('jun', 'jul', 'aug')='summer' ")

table(bank1$season) # a lot more contact in spring and summer 

# The problem: Can't transform a variable. Getting an error message. For example, 

log(bank$balance) 
sqrt(bank$balance) 

# The issue are the negatives and zeros. How many do we have? 
bank$balance[bank$balance<=0] %>% length/nrow(bank) # Quite a few, and it makes sense, so #16% ve begative zero balnce its possible# i can remove

bank$balance_std <- bank$balance+abs(min(bank$balance))+1 #min bank bal was -3313 tajing 3313 addig it still ll giv 0 so add +1 n add that to allvalues# simple stadardization of balance to beomce positive 

head(bank$balance); head(bank$balance_std)

# Now try the transformation 
log(bank$balance_std) # yes! 


## Logistic regression (cont.) 

# Stopped at exploring interactions with logit 

#################
# Find a "good" model to predict quitting.
# Your model might include interactions 
# Interpret the results. 
#################
quitmod <- glm(ynquit~jobsatisfaction*age, data=quit, family="binomial") #job satisfcation likert scake n not cont var#: adds only interaction * adds main effect n inetraction
summary(quitmod) 
#use exp to interpret results
plot(effect(term="jobsatisfaction:age", mod=quitmod, default.levels=20), multiline=T) # intersting #plot only interaction #job satisfaction has no effect fr age 60#effect of JS decreases the older u get on d odds of quitting

## 4. Diagnostics 
#= 
# Logit does not have exactly the same diagnostics tools as OLS. 
# But here is what we can do: 

# a. Examine for possible multicollinearity
vif(quitmod)
sqrt(vif(quitmod))>2 # There is multicollinearity in this model! 
# However, this is not data-related multicollinearity. This type of structured-multicollinearity, is less concerning. #we intrducd the interaction acknowledge n move on# if vif was der fr age n JS then need to consider#dont panic if cuz of inetraction term
# Still, know that coefficients are not going to be precise. 

# b. Check for overdispersion
# The logit model assumes a binomial distribution. 
# Sometimes the variance of the DV is larger than 
# specified for a binomial distribution. 
# The is called *overdispersion* (we don't typically find underdispersion)
# We identify overdispersion by dividing the residual deviance
# with the residual degrees of freedom in the model. 
# If the result is *much higher than 1*, there is overdispersion. #run chi sq n compare nt sure

# Returning to the bank model
bank_mod3 <- glm(y~balance+marital+loan+poutcome, data=bank1, family=binomial)

deviance(bank_mod3)/df.residual(bank_mod3) # Not bad. #deviance /residul the value is less than 1 so no overdispersion #we can test fr this using chi sq too

# But how do we know for sure? 
# We compare the model to a model that assumes overdispersion, 
# and see if there is a difference 

bank_mod3_od <- glm(y~balance+marital+loan+poutcome, 
data=bank1, family=quasibinomial) # note the quasibinomial distribution
                    
pchisq(summary(bank_mod3_od)$dispersion * bank_mod3$df.residual,
       bank_mod3$df.residual, lower=F) 
# p-value is much higher than 0.05 #if value is less than .05 then tehre is overdispersion #anything above std p=value no overdispersion
# There is no overdispersion. 

# If there is overdispersion, fit a quasibinomial distribution intead of binomial. 

#################
# Using the quit data, build a model 
# Check for multicollinearity
# Check for overdispersion
# What did you find? 
#################
rm(list=ls()) 
# Model fit 
#= 
# Instead of Adjusted-R-Squared we get AIC. 
# AIC: Index of fit that penalizes the number of parameters. 
# It is computed as: 
# -2*(max log-likelihood)+2*(number of parameters)
# So, the smaller the AIC, the better. 
# We can compare models using AIC
# but we don't get a good sense of our model's performance

# Instead, we use pseudo-R-Squared measures. 
# There are a few of them. Here's a good one from the rms package: 
mod_fit_3<- lrm(y ~ balance+marital+loan+poutcome, 
              data = bank1)
mod_fit_3$stats["R2"]  

# What if we take out one IV, like poutcome?
mod_fit_3.1 <- lrm(y~balance+marital+loan, 
			data = bank1) 

mod_fit_3.1$stats["R2"]  #pseudo r sq dropped from 13 to 3% #but cant tel a story 

# poutcome, despite having few values in sucess, is an important IV! 
# We could have also figure this out but simply looking at the z-value
summary(bank_mod3) 

#################
# Build three models and find the one with the best fit. 
# What are the best AIC?
# What is the best pseudo-R-Squared? 
#################

## BACKGROUND ON (EXPLORATORY) FACTOR ANALYSIS  #common #other type is conformity factor analsis rare sorta lik tree
#= 
# Aims to find underlying (latent) factors #obj is to find hidden structure in data
# that account for observed relationships 
# among numeric variables 
# Used to reduce variables, and build a scale/index (e.g., social status scale, work personality index) 

# The process is: 

#1. Examine the data 
#2. Scale the data (if needed) 
#3. Consider number of factors 
#4. Extract factors 
#5. (optional) use the factors discovered in analysis and modeling 


#1. Examine the data 
#= 
View(brands) 
str(brands)
summary(brands) 


#2. Scale the data (if needed) #change value but not distribution #not ransformed
#=
# Data may have different scales/values. We want to center them (xi-mean(x)). 
# Better yet, standardize them. This is how we standardize a variable: 
#(xi-mean(x))/sd(x)  #most common
# Or use R's scale function #all values seems to be likert and looks lik ll be simiar n can be grouped # diff scale lik 1 to mil n others in diff scale 

# This not needed when the variables have the same scale, as in the brands data. 
# But, this is the general procedure: 

brands_s <- data.frame(scale(brands[,-1])) # omit the brand variable (factor) 

describe(brands_s)  # as expected #with just describe mght get diff results clashes with other package so change code as pstch::describe
corrplot(cor(brands_s), method="circle", addCoef.col="grey", type="upper") #find var which correlate with one another but not one which is super correlated(lik both same) n dont want with no correlation we want .5 .7 etc

# We are looking for variables with relatively high correlation with one or a few others
 
# brands_s$brand <- brands$brand # add the factor variable 


#3. Consider number of factors #tell how many factors to extract
#=

# We'll use the fa.parallel function from the psych package 
# To use the current data against simulated data 
# The procedure in FA is to "rotate" the data to maximize variation for each factor. 
# Default rotation is varimax. Oblique rotations allow correlation among factors, whereas orthogonal rotations do not. 

fa.parallel(brands_s, fa="both", n.iter=100, show.legend=F) #its gonna rotate columns together to c if there is correlation w usualy do varimax. we here trying to max the dist betn them #diff rotation alogorithms exst
#our obj here is least no.of factors which gives most variance
#eigen value to look at distance

# Results suggest the optimal number of factors based on the data 
# Look for an "elbow", especially around eigenvalue = 1.  #it ll giv a reco itslf

## Perform FA 
#= 
# Many methods to extract factors (unlike PCA)
brandsfa1 <- factanal(brands_s, 3) #factor analysis #rem it has to be scaled data # here we ve same scale but still we use this
brandsfa1$loadings
# loading values are essentially correlation coefficient of variable with factor 
# The higher the loading, the higher the "correlation" with the factor 
# Look for loading of .5 or higher (conservatively, .7 or higher) 
# When developing a scale, you might include the negative with the positive loadings
# But typically use either all the positive, or negative that have a high loading 

# So, 
# Factor1 (bargain, value) - value proposition etc
# Factor2 (perform, leader, serious) - quality proposition #notice the fun variable 
# Factor (latest, trendy) -hip factor
#if i get -.8 #include either all + or all -
#create separate  with value proposition, , etc n also other left out var n then the story i can tel ll be much easier

#loadings which is above .7 can be conidered factor those tat didnt come are indepfrom the rest #prop var --.206 factor 1 explains 20% of variance
##############################################
# Perform factor analysis on the decathlon data. 
# How many factors were discovered?
# Which events loaded the best on these factors?  
# Can you come up with names for these underlying factors? 
##############################################

## ADDITIONAL RESOURCES
#=
# Books: 
#-
# Multiple Factor Analysis by Example Using R. 2014. Chapman and Hall/CRC 
# Exploratory Multivariate Analysis by Example, 2nd. Edition. 2017. CRC Press. 


# The above procedure is for exploratory factor analysis. If you want confirmatory analysis, use structural equation modeling (from the sem package) 
# A good tutorial of structural equation modeling using R is found at: http://socserv.socsci.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf




 