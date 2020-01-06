emp$department <- as.factor (emp$department)
emp$travel <- as.factor (emp$travel)
emp$quit <- as.factor (emp$quit)
str(emp)
table(emp$department)
emp$department <- as.factor (emp$department)
emp_factors <- c("department", "gender", "marital", "overtime", "quit", "travel")
emp[emp_factors] <- lapply(emp[emp_factors], factor)
emp1 <- emp[,!emp_factors]
corrplot()
corrplot(cormat)
describe(emp1)
fa.parallel(emp, fa="both", n.iter=100, show.legend=F) 


#factor analysis - 1st step scale it. else it wont rotate data properly which has high variance
#we can scale only nueric data
#do describe just to make sure everything is fine
#fa.parallel to see no.of factors . it suggests 3 factors. now we need to extract the factors n c d loadings
#beyond .4 shud be correated with factor exam time pressue need not name #factor1 experience  #factor2 role index #seniority, monthly - they r correlated so dono if we want to extract
#extracting factors n building index

emp_nums <- emp[sapply(emp,is.numeric)]
cormat <- cor(emp_nums)
#corrplot:: 
#first add all other than the factors too many der cuz for factor analysis #addd index n after add leftover. name as factor1 too in time pressure
  
  emp_mod1 <- glm(quit~age+travel+marital+job_satisfaction+miles_from_home+years_since_promotion+pct_salary_increase+seniority+overtime+monthly, data=emp, family=binomial)

summary(emp_mod1)
#overtimeYes ***, job satisfcation, overtimeyes, travel none, marital single
factor3 <- emp[,""]
emp_mod2 <- glm(quit~age+travel+marital+job_satisfaction+miles_from_home+years_since_promotion+overtime+monthly, data=emp, family=binomial)

summary(emp_mod2)
emp_mod5 <- glm(quit~travel+marital+job_satisfaction+job_involvement+past_firms+miles_from_home+years_since_promotion+overtime, data=emp, family=binomial)
summary(emp_mod5)
emp_mod3 <- glm(quit~., data=emp, family=binomial)
summary(emp_mod3)
vif(emp_mod3)
sqrt(vif(emp_mod3))>2 
#too few levels of factors
#exlore interactions it may matter. if tme was der will explore interactions
#do exp and do the interpretations
#log odds after exp it is odds every unit of exp decreases d odd of quitting the job by less than 1%  76% less
#reduces by 40% travel
#single multiplied by 2.7
#check fr dispersion
#2 models check aic check pseudo rsq nwhch model is bter
#interpret effects most imp factor is __ talk abt co-eff
#mention problems in diagnostics