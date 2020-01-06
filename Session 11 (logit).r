#=========================================================
# IDS 462, Session 11 - Logit 
#=========================================================
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================
rm(list=ls()) 
## Load libraries 
#=
library(car)
library(Deducer)
library(tidyverse)
library(rms)

## Load data 

# Same data as last time, Session 10 

## Logit Regression 
#==================

# We are modeling the odds of (y), which is a binary outcome variable  
# In this case, a client's acceptance of a product promotion

## Some preliminaries 
#=
# With logistic regression (logit) we model the *log odds* of an outcome=1 
# As in: got hired; quit job; purchased product; visited website; admitted to hospital; credit card application approved... you've got it! 
# We model the log odds of an event from a binomial distribution, where the probability of 1 (TRUE/HIGH/SUCCESS...) is found by a transformation of a linear model of the IVs

# How it works: GLM
# (1) probability distribution of an event
# (2) a linear model
# (3) a function that "links" the model to the parameter of the predicted distribution 
# Logit: (1) probability (0-1); (2) model with numeric and factor IVs; (3) links to a value between -inf to inf 

# Let's plot the logit function (for the log odds of an event)

curve((1/(1+exp(-x))),-10,10, main="Logit Function",
col ="steelblue", lwd=3)

# Assumptions:
# Like OLS
# - Independence of errors (no repeated measures, for example) 
# - Predictors should not be highly correlated with each other (multicollinearity). 

# Unlike OLS 
# There is no assumption of nomral distribution. 
# Assumes linear relationship between continuous IV and the *logit of DV*. 

## Step 1. Examine data, clean if needed 
#=
# In the bank data our DV is y 
# And the IVs are: age, balance, education, marital status, loan, default)

# inspect the data
head(bank)
tail(bank)
bank %>% sample_n(20) %>% View() 
# or using R base: View(bank[sample(nrow(bank),10),]) 
#y column is whether they ccepted or not
## Step 2: Examine at a univariate level (transform if needed) 
#=
# Outcome variable 
tab <- table(bank$y)
#table(bank$y) %>%prop.table %>% round(2)
barplot(tab, col=c("orange", "steelblue"), main="Accepted Offer")

# Predictor variables 
# this step is to select relevant IVs and prepare them as needed 

str(bank)

# job will be hard to used in a model, and month is a bit on an odd variable
#job 12 level so drop cuz diff in regrn model to work with them #if have time change it into blue or white collared


# Also pay attention to highly skewed variables. 
# While there is no assumption of linearity, there is an assumption about linearity with logit of the DV. 
# It is best to transform, or remove extrme outliers (high leverage observations). 


## Step 3: Examine bivariate relations with relevant IVs and DV 
#=
# Default might be relevant 

table(bank$default) # very few default cases 

# still, is there a relationship? 
xt_def_y <- xtabs(~y+default, data=bank) #xtabs fr header
summary(xt_def_y) # chisq.test; nope 

# Education? 
xt_educ_y <- xtabs(~y+education, data=bank)
summary(xt_educ_y) # chisq.test; better!  

# We also want to explore a balance (numeric) 
plot(density(bank$balance))
boxplot(bank$balance~bank$y, 
  main="Relationship between balance and offer acceptance", 
  col=rainbow(2))  # both are skewed #using comparitive boxplot diff in balance of thse who accepted n not
# outlier = 75% + 1.5IQR
outlier <- quantile(bank$balance, probs = c(0.75))+1.5*IQR(bank$balance) #over 73
# how many do we have 
balance_outliers <- which(bank$balance>outlier)
balance_outliers
length(balance_outliers)/nrow(bank)
bank1 <- bank[-balance_outliers,]
plot(density(bank1$balance)) # 

boxplot(bank1$balance~bank1$y, 
  main="Relationship between balance and offer acceptance", 
    col=rainbow(2))  # better #wo outliers #from the boxplot we see tat ppl who accept offer have higher balance

#################
# Examine the quit data 
# And consider univariate and bivariate relationships 
# With ynquit 
#################

## Step 4. Modeling  
#=

bank_mod1 <- glm(y~education+balance, data=bank1, family=binomial) #binomial approximates logit
#glm(y~education+balance, data=bank1, family=binomial) %>% summary #if u dont want to save model n just view
summary(bank_mod1) #fisher iterations #it tries to fit #how many tmimes # smtimes cant fit

# Interpretation
# Look at the p-values (and asterisks) to decide 
# which variables are significantly related to the DV
# For each significant variable, interpret the "meaning." 
# This stage is more difficult compared to OLS because the coefficients are log odds 
# For example, having taken a loan bank 
# reduces the log odds
# but log odds are hard to understand #co-effs r log odds

# a. It is easier to convert log odds to odds ratios
# using the exponent function 
exp(coef(bank_mod1)) #log odds cant interpret so taking exp #.578 increase #very dollar in balance increases the odds of accepting the offer by .000 on avg by holding other var constnt. multiplies the odds by 1.00
# For example, tertiary education multiplies the odds of y by about .58,
# compared to people with primary education, 
# holding the other IVs at their constant  

# b. It is often more useful to also get confidence intervals 
# for predictions
exp(confint(bank_mod1, level=.99)) # default is .95 #here doing 99% conf level#edu multip by .4199 to twice
# So, for those who took a loan, the odds multiply between .3 and .7 
# compare to those who did not take a loan. 

#################
# Select two IVs in the quit data 
# Interpret the odds for quitting for a model with both IVs
# Interpret the odds with confidence intervals  
#################

#################
# Select two IVs in the quit data 
# Interpret the odds 
# Interpret the odds with confidence intervals  
#################
	
# Add more variables 
	
bank_mod2 <- glm(y~balance+education+age+marital+loan+poutcome, 
	                 data=bank1, family=binomial)
summary(bank_mod2)
#if poutcome success increases the odds of accepting loan
#being married reduces the odds of accepting the loan

# Let's check if the second model is significantly better: 

# First, look at AIC 
# The lower AIC the better fit. More on this later. 

#second model aic dropped by 200 so its betr go with it
#read 382 ch-8
# Second, test the difference 
anova(bank_mod1, bank_mod2, test="Chisq") 
  # only if we the first mode uses variables in the second  
  # answer: yes 

# Now re-run the previous procedure 
exp(coef(bank_mod2))
 # Tertiary education is still significant (p<0.01), 
 # has changed a little, dropped to .47 from .58 in the first model. 
 #poutcome success has big co-eff so sig #odds multipled by 11

  # balance is also significant (p<0.01)
 # an increase in one unit of balance (assume 1 $USD), 
 # increases the odds of y by 0.02% on average.  
 # so, $1000 years increases the odds 
 # by: (1.00022185^1000) = 1.25, on average. 

# Depdning on number of cases in the data, I prefer IVs with p-value<0.01 
bank_mod3 <- glm(y~balance+marital+loan+poutcome, data=bank1, family=binomial)
summary(bank_mod3)

# Test the difference 
anova(bank_mod2, bank_mod3, test="Chisq") 
#this model less variables so betr to go with it

# Predict outcome #we r going to get odds so ll be betn 0 n 1
set.seed(123) 
samp <- sample(1:nrow(bank), 100) 
pred <- predict(bank_mod3, 
        bank[samp, c("balance", "marital", "loan","poutcome")], 
        type="response") %>% round(1) #predict fn dont forget type response #instead of eyeballing select greater than .5
ylikely <- which(pred>0.5) %>% names %>% as.numeric # more likely to accept the offer 
bank[ylikely,c("balance", "marital", "loan", "poutcome", "y")] %>% View 
#acceptance is low
#our model has issues #only captured 2 

#################
# Select others IVs, including the two selected earlier
# from the quit data 
# Is there a difference between the reduced, to the fuller model? 
# Have any significance levels changed (e.g., from significant to not significant)? 
# Interpret the odds with confidence intervals
# Compare the with the reduced model 
# Predict the probability of quitting in the first 5 cases 
#################

# Interactions?
  # very much like OLS 
bank_mod4 <- glm(y~marital+loan+balance*poutcome, data=bank, family=binomial)

summary(bank_mod4) # This interaction term is not quite statistically significant. We will omit it. 

#################
# Find a "good" model to predict quitting.
# Check for possible interactions.  
# Interpret the results. 
#################

