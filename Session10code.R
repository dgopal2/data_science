#=========================================================
# IDS 462, Session 10 - (Wrapping up OLS), Logit 
#=========================================================
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#====================================
library(effects)

load("Session10data.RData")  

### Finish OLS 
  
mod2_terms<-lm(log(price)~lotsize+bedrooms+airco+lotsize:prefarea, data=realestate) # adding an interaction term of lotsize and prefarea
#prefarea doesnt ve main effect here but only has a stat sig intercation effect tats y it was inclded. we use log fr price to make it normal dist cuz assumption of ols is normal dist
summary(mod2_terms) # Something is certainly going on here #adj rq is given here itself
#interpret as log(price)
#here we wanna c if lotsize really has effect or cuz of interaction with pref area the lotsize is having effect
plot(effect(term="lotsize:prefarea", mod=mod2_terms, default.levels=20), multiline=T) # same plot 
#wat we look here is - is there a diff slope? if so yes. Here bigger lotisize houses is pref area increases their price soon. intercation effect. so wen both lotsize n pref area meets has highr slope


# Another example of interaction of two factors 
mod3_terms<-lm(log(price)~lotsize+prefarea*recroom, data=realestate) # adding an interaction term and main effect using the * symbol 
summary(mod3_terms) 
plot(effect(term="prefarea*recroom", mod=mod3_terms, default.levels=20), multiline=T) # same plot 
#this is a 2 factor interaction n they r simple factors yes or no.
#here we put multiply sign
#interpretation the co-eff r small cuz of log(price). we need toexponentiate bt exam no time so it has .0007 units of log price; each sqft of lotsize increases the?? log price by ??
#prefrareayes has a effect on 25% higher as compared to not being in pref area
#the log price is higher bby 25%
#being in a recroom has a effect 
#when having a recroom, the log price is higher bby 25% when compared to not havng a recroom
#prefarea n recroom has a slight sign interaction #when a house is in prefarea and has a recroom, the log price decreases by 15% as compared to not having both recroom and not being in prefarea
#wehn  lots of levels r dr more complex right now ignore n also wen number n factor #focus on sign n stars
#adj rsq the indep var explains variance in log price by 38%
mod3<-lm(log(price)~lotsize+prefarea+recroom, data=realestate) # adding an interaction term and main effect using the * symbol 
summary(mod3) 
#now adj r2 is 38 adding interaction terms increased the model performance by 1% which is not much. #in exam simply pay attention to it
#its because the interaction p-value was one star

# Diagnostics 
#=
plot(mod3_terms)#gives 4 plots # This is a great way to identify multiple possible concerns with the model 
# For example, outliers. 
#always ll ve residual i want to make sure residual is normally distributed
#initially saw outlier fr univariate now seeing at model level
# Better yet: 
par(mfrow=c(2,2)) # This function sets up the plotting canvas to 2x2, so that we don't need to go through them one by one. Now again:  
plot(mod3_terms)

# We could also plot diagnostics "manually", such as: #similare plots using diff codes
dev.off() 
plot(predict (mod3_terms), residuals (mod3_terms)) 
plot(hatvalues(mod3_terms)) 
identify(hatvalues(mod3_terms), col="steelblue") # Neat, but this interactive feature doesn't always work, so: 
tail(sort(hatvalues(mod3_terms)), n=5)

# Cook's distance indicates other "suspects" as well. We need to examine them and decide what to do with these cases
realestate[c(103,104,414), c("price", "lotsize", "recroom", "prefarea")]
#414 lotsize more n price is so less
#103, 104 highre price tats y 

# We could remove these observations, and re-run our model 
outliers <- c(103,104,414)
realestate_new <- realestate[-outliers,]
nrow(realestate_new)-nrow(realestate)  
#removed the 3rows
mod3_terms_1<-lm(log(price)~lotsize+prefarea*recroom, data=realestate_new) 
summary(mod3_terms_1)
# Did our model improve? 
# We want to know to find the extent to which our models have improved, after "cleaning"
summary(mod3_terms_1)$adj.r.squared-summary(mod3_terms)$adj.r.squared  
#removing 3 outliers from 500 cases improved our model by 2%
#here cook's distance model didnt help so here we focusing on residuals mode3_terms_n1 was done by removing 93, 365 based oncook's distance

# Check for multicollinearity
sqrt(vif(mod3_terms_1))>2 # car package is needed fr vif. Looks fine. If TRUE (vif > 4), consider interaction effect. If marginal, drop. 
# But some multicollinearity is expected when interactions are present. If no interactions in model, address variables and re-run model. 
#dont worry abt inetraction term other iv shud be false

#################
# From the bank data set  
# Build a *good* model for the balance variable 
# Focus on positive balance that is less than $10k
#################


## Logistic Regression  

## Load libraries 
#=
library(car)
library(Deducer)
library(tidyverse)
library(rms)

# We are modeling the odds of (y) 
# a client's acceptance of a product promotion

## Some preliminaries 
#=
# With logistic regression (logit) we model the *log odds* of an outcome=1 
# As in: got hired; quit job; purchased product; visited website; admitted to hospital; credit card application approved... you've got it! 
# We model the log odds of an event from a binomial distribution, where the probability of 1 (TRUE/HIGH/SUCCESS...) is found by a transformation of a linear model of the IVs
# As opposed to OLS, there is no assumption of nomral distribution. These models are called Generalized Linear models (glm). 
# GLM: (1) probability distribution of an event; (2) a linear model; (3) a function that "links" the model to the parameter of the predicted distribution 
# Logit: (1) probability (0-1); (2) model with numeric and factor IVs; (3) links to a value between -inf to inf 

# Let's plot the logit function (for the log odds of an event)

curve((1/(1+exp(-x))),-10,10, main="Logit Function",
col ="steelblue", lwd=3)

## Step 1. Make sure the data are clean 
#=
# let's assume that I want to predict y 
# based on: age, balance, education, marital status, loan, default)

# inspect the data
head(bank)
tail(bank)
bank %>% sample_n(20) %>% View() 
# or using R base: View(bank[sample(nrow(bank),10),]) 


## Step 2: Examine at a univariate level (transform if needed) 

str(bank)
# job will be hard to model, and month is a bit on an odd variable 

# Outcome variable 
tab <- table(bank$y)
barplot(tab, col=c("orange", "steelblue"), main="Accepted Offer")

## Step 3: Examine bivariate relations 

# y and education 
t_educ_y<-table(bank$education, bank$y) 
p_educ_y <- prop.table(t_educ_y)
addmargins(p_educ_y,c(1,2))

# how about a financial variables, such as default 
t_default_y<-table(bank$default, bank$y) 
p_default_y <- prop.table(t_default_y)
addmargins(p_default_y, c(1,2))

# better yet
xt_def_y <- xtabs(~y+default, data=bank)
xtp_def_y <- round(prop.table(xt_def_y), 3) 
addmargins(xtp_def_y, c(1,2))
summary(xt_def_y)

# We also want to explore a factor DV with a numeric IV 
plot(density(bank$balance))
boxplot(bank$balance~bank$y, 
  main="Relationship between balance level and age", 
  col=rainbow(2))  # both are skewed 
# outlier = 75% + 1.5IQR
outlier <- quantile(bank$balance, probs = c(0.75))+1.5*IQR(bank$balance) #over 73
# how many do we have 
balance_outliers <- which(bank$balance>outlier)
balance_outliers
length(balance_outliers)/nrow(bank)
bank1 <- bank[-balance_outliers,]
plot(density(bank1$balance)) # 

boxplot(bank1$balance~bank1$y, 
  main="Relationship between balance level and age", 
  col=rainbow(2))  # better 

# or we could have simply used a transformation
car::powerTransform(bank$balance+abs(bank$balance)+1) # pretty close to log 
bank$scaled_balance <- (bank$balance+abs(min(bank$balance))+1)^0.17
# Still not ideal 

#################
# Examine the quit data 
# And consider univariate and bivariate relationships 
# With ynquit 
#################

## Step 4. Modeling  
#=

# Background on logistic regression

bank_mod1 <- glm(y~marital+loan+default, data=bank, family=binomial)

summary(bank_mod1)

# Interpretation
# Look at the p-values (and asterisks) to decide 
# which variables are significantly related to the DV
# For each significant variable, interpret the "meaning." 
# This stage is more difficult compared to OLS because the coefficients are log odds 
# For example, having taken a loan bank 
# reduces the log odds
# but log odds are hard to understand 

# a. It is easier to convert log odds to odds ratios
# using the exponent function 
exp(coef(bank_mod1))
# For example, taking a loan multiplies the odds of y by about .64,
# compared to those who did not take a loan, 
# holding the other IVs at their constant  

# b. It is often more useful to also get confidence intervals 
# for predictions
exp(confint(bank_mod1, level=.99)) # default is .95
# So, for those who took a loan, the odds multiply between .3 and .7 
# compare to those who did not take a loan. 

#################
# Select two IVs in the quit data 
# Interpret the odds for quitting for a model with both IVs
# Interpret the odds with confidence intervals  
#################
