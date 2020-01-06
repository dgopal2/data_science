# Outcome variable 
tab <- table(quit$ynquit)
tab
#table(quit$ynquit) %>%prop.table %>% round(2)
barplot(tab, col=c("orange", "steelblue"), main="Accepted Offer")

# Predictor variables 
# this step is to select relevant IVs and prepare them as needed 

str(quit)
quit$jobsatisfaction <- as.factor(quit$jobsatisfaction)
table(quit$jobsatisfaction)
table(quit$age)
table(quit$children)
table(quit$education)
quit$education <- as.factor(quit$education)
# job will be hard to used in a model, and month is a bit on an odd variable
#job 12 level so drop cuz diff in regrn model to work with them #if have time change it into blue or white collared


# Also pay attention to highly skewed variables. 
# While there is no assumption of linearity, there is an assumption about linearity with logit of the DV. 
# It is best to transform, or remove extrme outliers (high leverage observations). 


## Step 3: Examine bivariate relations with relevant IVs and DV 
#=
# Default might be relevant 
chisq.test(quit$ynquit, quit$yeartenure)
chisq.test(quit$ynquit, quit$jobsatisfaction)
chisq.test(quit$ynquit, quit$gender) #NS
chisq.test(quit$ynquit, quit$children) #sig
str(quit)
summary(aov(yeartenure ~ ynquit, data=quit)) #sig
summary(aov(age ~ ynquit, data=quit)) #NS
table(bank$default) # very few default cases 

#2
quit_mod1 <- glm(ynquit~children+yeartenure, data=quit, family=binomial) 
summary(quit_mod1) 
#childrenyes  0.48461    0.27782   1.744   0.0811 .  
#yeartenure   0.03867    0.02059   1.878   0.0604 .
exp(coef(quit_mod1))
#(Intercept) childrenyes  yeartenure 
#0.1651126   1.6235490   1.0394306 
#every one year increase in tenure multiplies the odss of quitting by 1 holding all other variables constant
#one unit of increase in job satisfaction (keeping it as numeric betr) (we r not sure of unitsif evenly distributed)(betr to keep it as it is likert scale)
#having children multiplies the odds by
exp(confint(quit_mod1, level=.99))

#3
quit$jobsatisfaction <- as.numeric(quit$jobsatisfaction)
quit$education <- as.numeric(quit$education)
quit_mod2 <- glm(ynquit~children+yeartenure+jobsatisfaction+age, data=quit, family=binomial) 
summary(quit_mod2) 

exp(coef(quit_mod2))
anova(quit_mod1, quit_mod2, test="Chisq") 
set.seed(123) 
samp <- sample(1:nrow(quit), 100) 
pred <- predict(quit_mod2, 
                quit[samp, c("children", "yeartenure", "jobsatisfaction","age")], 
                type="response") %>% round(1) #predict fn dont forget type response #instead of eyeballing select greater than .5
ylikely <- which(pred>0.2) %>% names %>% as.numeric # more likely to accept the offer 
quit[ylikely,c("children", "yeartenure", "jobsatisfaction","age", "ynquit")] %>% View 