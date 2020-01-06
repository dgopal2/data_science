quitmod1 <- glm(ynquit~jobsatisfaction*yeartenure, data=quit, family="binomial") #job satisfcation likert scake n not cont var#: adds only interaction * adds main effect n inetraction
summary(quitmod1) 
plot(effect(term="jobsatisfaction:yeartenure", mod=quitmod1, default.levels=20), multiline=T) # intersting #plot only interaction #job satisfaction has no effect fr age 60#effect of JS decreases the older u get on d odds of quitting

quitmod2 <- glm(ynquit~children*yeartenure, data=quit, family="binomial") #job satisfcation likert scake n not cont var#: adds only interaction * adds main effect n inetraction
summary(quitmod2) 
plot(effect(term="children:yeartenure", mod=quitmod2, default.levels=20), multiline=T) # intersting #plot only interaction #job satisfaction has no effect fr age 60#effect of JS decreases the older u get on d odds of quitting
#model 2 is betr
#aic value #with js betr but not a good story
#strong predictor n strong story can work togeher or not
#did a model with just JS n checked aic z value

vif(quitmod1)
sqrt(vif(quitmod1))>2
# Returning to the bank model
q1 <- glm(ynquit~jobsatisfaction+children+age+gender+yeartenure+education, data=quit, family=binomial)

deviance(q1)/df.residual(q1)  #thereis overdisersion


q2 <- glm(ynquit~jobsatisfaction+children+age+gender+yeartenure+education, 
                    data=quit, family=quasibinomial) # note the quasibinomial distribution

pchisq(summary(q2)$dispersion * q2$df.residual,
       q2$df.residual, lower=F) #no overdispersion



#decathlon
View(decathlon) 
str(decathlon)
summary(decathlon) 

d1 <- data.frame(scale(decathlon[,1:10])) # omit the brand variable (factor) 

describe(d1)  # as expected #with just describe mght get diff results clashes with other package so change code as pstch::describe
corrplot(cor(d1), method="circle", addCoef.col="grey", type="upper") #find var which correlate with one another but not one which is super correlated(lik both same) n dont want with no correlation we want .5 .7 etc

# We are looking for variables with relatively high correlation with one or a few others



brands_s$competition <- brands$competition

fa.parallel(d1, fa="both", n.iter=100, show.legend=F) #its gonna rotate columns together to c if there is correlation w usualy do varimax. we here trying to max the dist betn them #diff rotation alogorithms exst
