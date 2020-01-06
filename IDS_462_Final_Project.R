
#----------------------------------- IDS 462 Final Project  -------------------------------

# Group 1

# Team members:
# Adewojo	Adeyinka
# Ganesan	Mahaalaxmi
# Misra	Harsh
# Pai Aparna
# Subramaniyan Sivaganesh
# Birla Sumeet 


#----------------------------------- Importing required libraries  -------------------------------
library(dplyr)
library(car) 
library(tidyverse)
library(lubridate)
library(stringr)
library(corrplot)
library(gmodels)
library(psych)
library(rpivotTable)
library(corrplot) 
library(gmodels)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(plotly)
library(RColorBrewer)
library(vcd)
library(corrplot)
library(ggfortify) 
library(effects)
library(rms) 
library(e1071)
library(caret)
library(rockchalk)
#----------------------------------- Importing the dataset  -------------------------------
laptop12 <- read_csv("laptops.csv")
brand <- read_csv("laptops_brand.csv")
laptop <- merge(laptop12,brand,by="Company")
#Removing Irrelevant columns

laptop$Rank <- NULL
laptop$BrandValue <- NULL
laptop$Country <- NULL
laptop$Continent <- NULL
laptop$`Laptopmag ranking` <- NULL
laptop$X1 <- NULL
laptop$Product <- NULL


#----------------------------------- Checking datatypes  -------------------------------
glimpse(laptop)


#----------------------------------- Correcting datatypes  -------------------------------

laptop$Company <- as.factor(laptop$Company)
laptop$TypeName <- as.factor(laptop$TypeName)
laptop$Ram <- as.factor(laptop$Ram)
laptop$OpSys <- as.factor(laptop$OpSys)

#Extracting only numeric values from "Weight" column
numextract <- function(string){ 
                str_extract(string, "\\-*\\d+\\.*\\d*")
} 
laptop$Weight <- numextract(laptop$Weight)
laptop$Weight <- as.numeric(laptop$Weight)

#Extracting only resolution values (like 1920x1080) from "ScreenResolution" column
laptop$ScreenResolution <- word(laptop$ScreenResolution,-1)
laptop$ScreenResolution <- as.factor(laptop$ScreenResolution)

#Splitting Cpu column into 2 : Cpu (Company name) and CpuClockSpeed (X Ghz).
laptop$CpuClockSpeed <- word(laptop$Cpu,-1)
laptop$CpuClockSpeed <- numextract(laptop$CpuClockSpeed)
laptop$CpuClockSpeed <- as.numeric(laptop$CpuClockSpeed)


laptop$Cpu <- word(laptop$Cpu, 1, sep=" ")
laptop$Cpu <- as.factor(laptop$Cpu)

#Splitting Memory column into 2 : Memory (8GB, 16GB) and MemoryType (HDD / SSD/ Flash)
laptop$MemoryType <- word(laptop$Memory,2)
laptop$MemoryType <- as.factor(laptop$MemoryType)

laptop$Memory <- word(laptop$Memory,1)
laptop$Memory <- as.factor(laptop$Memory)

#Extracting only Gpu Company Name from Gpu column.
laptop$Gpu <- word(laptop$Gpu, 1, sep=" ")
laptop$Gpu <- as.factor(laptop$Gpu)

glimpse(laptop)

#----------------------------------- Checking for missing values -------------------------------

sapply(laptop,function(x) sum(is.null(x)))
#No Null values present.

sapply(laptop,function(x) sum(is.na(x)))
#7 NA values present in Brand Revenue column.

which(is.na(laptop$BrandRevenue)) #These values belong to companies : Chuwi and Vero. We'll remove these rows.
laptop <- laptop[-c(283,284,285,1296,1297,1298,1299),] 

#----------------------------------- Data Wrangling -------------------------------

Filter(is.factor, laptop)

#Creating a new column Origin (ASIA / USA)
laptop$Origin <- combineLevels(laptop$Company, levs = c("Acer","Asus","Fujitsu","Huawei","Lenovo","LG","MSI","Samsung","Toshiba","Xiaomi"),newLabel = "ASIA")
laptop$Origin <- combineLevels(laptop$Origin, levs = c("Apple","Dell","Google","HP","Mediacom","Microsoft","Razer"),newLabel = "USA")
laptop$Origin <- as.factor(laptop$Origin)

#Combining levels with very few data points together

laptop$TypeName <-combineLevels(laptop$TypeName, levs = c("Netbook","Workstation"), newLabel = "Netbook_Workstn")
laptop$ScreenResolution <- as.factor(ifelse(laptop$ScreenResolution == "1920x1080", "1920x1080", "Other"))

laptop$Ram <-combineLevels(laptop$Ram, levs = c("2GB","4GB","6GB"), newLabel = "Less_than_8GB")
laptop$Ram <-combineLevels(laptop$Ram, levs = c("12GB","16GB","24GB","32GB","64GB"), newLabel = "More_than_8GB")

laptop$Memory <- as.factor(ifelse(grepl("GB",laptop$Memory), "Less_than_500GB", "More_than_500GB"))

laptop$OpSys <-combineLevels(laptop$OpSys, levs = c("Windows 10","Windows 10 S","Windows 7"),newLabel = "Windows_OS")
laptop$OpSys <-combineLevels(laptop$OpSys, levs = c("macOS","Mac OS X"),newLabel = "Mac_OS")

Filter(is.factor, laptop)
colnames(laptop)[12] <- "BrandRevenue"
# Brand Revenue column here corresponds to Brand size everywhere in the project. 

#----------------------------------- Data Transformation -------------------------------

Filter(is.numeric, laptop)

plot(density(laptop$Inches)) #We see that there are 3 modes. No action taken as we do not have continous values for Screensize .
plot(density(laptop$Weight)) #Bi-modal, slightly right skewed. No action taken as we do not have continous values for Weight .
plot(density(laptop$CpuClockSpeed)) #We see that there are 3 modes. No action taken as we do not have continous values for CpuClockspeed .

plot(density(laptop$Price_euros)) #Pretty-right skewed.
#Log transform of price variable
plot(density(log(laptop$Price_euros))) 
#Normally distributed now. We will use log(price) for all regression analysis.


#----------------------------------- Univariate Analysis -------------------------------

## Price_euros [DEPENDENT VARIABLE] : Price of a Laptop (Numeric)
options(scipen = 99)
hist(laptop$Price_euros, main="Distribution of Price", 
     col='lightblue', freq=F, xlab = "Price of a Laptop", breaks = 200)
lines(density(laptop$Price_euros, na.rm = T), col="red", lwd=3)
box(lty = '1373', col = 'red')

hist(log(laptop$Price_euros), main="Distribution of Price", 
     col="lightblue", freq=F,ylim = c(0,0.7), xlab = "Log of Price")
lines(density(log(laptop$Price_euros), na.rm = T), col="red", lwd=3)
box(lty = '1373', col = 'red')

# Initially Right Skewed distribution. The skewness is mostly because of the increased 
# number of notebooks in the dataset which are priced at a lower price. 
# Obtained a normalised graph for the Price distribution after log transformation,
# which would be used as a dependent variable for further analysis

# Type Name: Type ofthe laptop based on configurations

tab0 <-table(laptop$TypeName)
tab0
barplot(tab0,ylim=c(0,1000), axes=F, col='indianred3', main="Types of Laptop in the market")
axis(side = 2, at = seq(from=0, to=1000, by=200))
box(lwd=1.5)

ggplot(laptop, aes(x=TypeName, fill= TypeName)) + 
  geom_bar(position="dodge") + 
  labs(title="Types of Laptop in the market", x="Type", y="Count of Laptops") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "TypeName")

# Notebook are more in number compared to all the others may be because they are 
# priced at a lower price and serve the same purpose as that of the laptops. 
# Gaming and Ultrabook are almost same in number


# Origin: Manufacturing Place of Laptops 
tab1 <-table(laptop$Origin)
tab1
barplot(tab1,ylim=c(0,1000), axes=F, col='indianred3', main="Distribution of Manufacturing Origin of Laptops")
axis(side = 2, at = seq(from=0, to=1000, by=200))
box(lwd=1.5)

ggplot(laptop, aes(x=Origin, fill= Origin)) + 
  geom_bar(position="dodge") + 
  labs(title="Manufacturing of Laptops", x="Origin Continent", y="Count of Laptops") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Origin")

# The laptops manufactured in both the continents are almost similar although Asian 
# Laptops are more in number as comapred to USA laptops

# Operating System
tab2 <-table(laptop$OpSys)
tab2
barplot(tab2,ylim=c(0,1500), axes=F, col='indianred3', main="Distribution of Operating System")
axis(side = 2, at = seq(from=0, to=1500, by=200))
box(lwd=1.5)

ggplot(laptop, aes(x=OpSys, fill= OpSys)) + 
  geom_bar(position="dodge") + 
  labs(title="Distribution of Operating System", x="OS", y="Count of Laptops") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "OS")

# The laptop having windows as their operating system are more than compared to laptops 
# having any other OS. 


#----------------------------------- Bivariate Analysis -------------------------------

## Distribution of price of laptop across Origins (Numeric and Factor Analysis)
describeBy((laptop$Price_euros) , laptop$Origin) # Statistical Analysis of price across Origin.
boxplot((laptop$Price_euros)~laptop$Origin, data=laptop, col=brewer.pal(10, "Set3"), main="Price of laptop across origin")
# Mostly eqaul distribution of price

upper_outliers <- fivenum(laptop$Price_euros)[4]+IQR(laptop$Price_euros)*1.5
which(laptop$Price_euros>=upper_outliers) 

# Mostly Gaming laptops are the once which are priced higher in both the continents

## Distribution of tranformed price of laptop across Origins (Numeric and Factor Analysis)
describeBy(log(laptop$Price_euros) , laptop$Origin) # Statistical Analysis of price across Origin Market.
boxplot(log(laptop$Price_euros)~laptop$Origin, data=laptop, col=brewer.pal(10, "Set3"), main="Log of Price of laptop across origin")
# This shows a more defined differentiation. The log of price has a slightly more 
# variation in Asian laptops as comapred to USA.The median is almost equal for both. 

## Distribution of memory across Category of Laptop (Factor and Factor Analysis)
tab3 <- table(laptop$Memory, laptop$TypeName)
barplot (tab3, main= "Distribution of memory across Category of Laptop", col=brewer.pal(10, "Set3"))
legend("right", 
       legend = rownames(tab3), 
       fill = brewer.pal(10, "Set3"))
box(lwd=1.5)

# stacked barplot 
ggplot(data=laptop) + aes(x=TypeName, fill=Memory) + 
  geom_bar(position="stack")+
  scale_fill_brewer(palette="Pastel1") + 
  labs(title='Distribution of memory across Category of Laptop',
       x="Type", y="Count of Laptops") 

# dodged (side-by-side) barplot 
ggplot(data=laptop) + aes(x=TypeName, fill=Memory) + 
  geom_bar(position="dodge")+
  scale_fill_brewer(palette="Pastel2") + 
  labs(title='Distribution of memory across Category of Laptop',
       x="Type", y="Count of Laptops") 

# Most of the laptops sold are less than 500 GB in all the categories. Only a higher % 
# of greater than 500 GB laptops are seen in case of notebooks. 
# Ultrabooks have low power processors and hence their memory is also less. Hence more than
# 99% of the ultrabooks are less than 500 GB.

## Preference of CPU and GPU brands across manufacturing markets
tab4 <- table(laptop$Origin, laptop$Cpu)
ggplot(data=laptop) + aes(x=Origin, fill=Cpu) + 
  geom_bar(position="stack")+
  scale_fill_brewer(palette="Pastel1") + 
  labs(title='Distribution of memory across Category of Laptop',
       x="Type", y="Count of Laptops") 

tab4 <- table(laptop$Origin, laptop$Gpu)
ggplot(data=laptop) + aes(x=Origin, fill=Gpu) + 
  geom_bar(position="stack")+
  scale_fill_brewer(palette="Pastel1") + 
  labs(title='Distribution of memory across Category of Laptop',
       x="Type", y="Count of Laptops") 

# These 2 graphs indicate that in terms of the processor intel is the most preffered brand 
# among all the laptop brands where as in terms of graphic cards Intel and Nvidia share
# the same market in Asian brands. The american brands still have intel as their major graphic
# card partners.

## Relation between cpu clock speed and Memory Type (Numeric and Factor Analysis)
describeBy(laptop$CpuClockSpeed , laptop$MemoryType) # Statistical Analysis of CPU speed and Memory Type.
boxplot(laptop$CpuClockSpeed~laptop$MemoryType, data=laptop, col=brewer.pal(10, "Set3"), main="Relation between CPU speed and Memory Type")

# The laptops having SSD hard drives have more CPU speed than all the others showing that nonvolatile flash memory
# is better than all the others. 

## Relation between Type of laptop and Screen Resolution (Factor and Factor Analysis)

tab5 <- table(laptop$ScreenResolution,laptop$TypeName)
barplot (tab5, main= "Distribution of resolution across laptop types", col=brewer.pal(10, "Set3"))
legend("right", 
       legend = rownames(tab5), 
       fill = brewer.pal(10, "Set3"))
box(lwd=1.5)

# stacked barplot 
ggplot(data=laptop) + aes(x=TypeName, fill=ScreenResolution) + 
  geom_bar(position="stack")+
  scale_fill_brewer(palette="Pastel1") + 
  labs(title='Distribution of resolution across laptop type',
       x="Type", y="Frequency") 

# 1920*1080 is the most used screen resolution irrespective of the class of laptops. 


# Summary of Bi-variate analysis:
#	Log of price is almost similar for US and Asian brand laptops with only a slight difference in median. 
# More than 50% of the laptops on the catalogue have less than 500 GB of memory. Notebooks have the highest number of laptops with more than 500 GB of memory.
# Intel is the industry leader for CPU as well as GPU.
# In Asia, Nvidia holds high market share.
# In USA, Nvidia and AMD hold similar market share.


#----------------------------- OLS Regression b/w Price & Laptop Specs------------------

summary(laptop)

## Dependent Variable is Price_euros
## To attain a more normally distributed variable, the log of Price_euro is used.

model1<-lm(log(Price_euros) ~ TypeName+ScreenResolution+Cpu+CpuClockSpeed+Gpu+Inches+Ram+Memory+OpSys+Weight+MemoryType+Origin+BrandRevenue, data=laptop)
summary(model1)
dev.off()
par(mfrow=c(2,2))
plot(model1)

# Adjusted RSquared is 77%

#Keeping only significant vars and rerunning the model
model2<-lm(log(Price_euros) ~ TypeName+Cpu+CpuClockSpeed+Ram+Weight+MemoryType+BrandRevenue, data=laptop)
summary(model2) 
dev.off()
par(mfrow=c(2,2))
plot(model2)
# Adjusted RSquared is 75%

# On average, the price of gaming, netbook_workstn & notebooks reduces while that of an
# Ultrabook increases as against the 2N1 laptop.

# On average, the price of a laptop with an Intel or Samsung processor increases by 
# the log of 40 cents as against a laptop with an AMD processor

# On average the price of a laptop increases by the log of 49 cents for every increase 
# in clocking speed unit

# On average, the price of a laptop with less than 8GB of RAM reduces by the log of 
# 37 cents while that of a laptop increases by the log of 34 cents as against a laptop with 8GB RAM

# On average, the price of a laptop increases by the log of 6 cents for every increase 
# in kg

# On average, the price of a laptop with Memory Types HDD, Hybrid & SSD increases as 
# against the Flash Memory type

# On average, the price of a laptop increases by the log of 0.01 for every unit increase
# in sale.



#------- Checking for Interactions--------
# Interaction 1 : TypeName:Weight
mod2_int <- lm(log(Price_euros)~TypeName+Cpu+CpuClockSpeed+Ram+Weight+MemoryType+BrandRevenue+TypeName:Weight, data=laptop)
summary(mod2_int)

plot(effect(term="TypeName:Weight", mod=mod2_int))
plot(effect(term="TypeName:Weight", mod=mod2_int, default.levels=20), multiline=T)
#Interaction is significant between Gaming, notebook and Netbook_Workstation. But doesn't make sense.


# Interaction 2 : TypeName:BrandRevenue
mod2_int2 <- lm(log(Price_euros)~TypeName+Cpu+CpuClockSpeed+Ram+Weight+MemoryType+BrandRevenue+TypeName:BrandRevenue, data=laptop)
summary(mod2_int2)

plot(effect(term="TypeName:BrandRevenue", mod=mod2_int2))
plot(effect(term="TypeName:BrandRevenue", mod=mod2_int2, default.levels=20), multiline=T) # same plot 
# No interactions

# Interaction 3 : CpuClockSpeed:Weight
mod2_int3 <- lm(log(Price_euros)~TypeName+Cpu+CpuClockSpeed+Ram+Weight+MemoryType+BrandRevenue+CpuClockSpeed:Weight, data=laptop)
summary(mod2_int3)

plot(effect(term="CpuClockSpeed:Weight", mod=mod2_int3))
plot(effect(term="CpuClockSpeed:Weight", mod=mod2_int3, default.levels=20), multiline=T) # same plot 
# There is an interaction but doesnt make any sense

# Interaction 4: CpuClockSpeed:MemoryType
mod2_int4 <- lm(log(Price_euros)~TypeName+Cpu+CpuClockSpeed+Ram+Weight+MemoryType+BrandRevenue+CpuClockSpeed:MemoryType, data=laptop)
summary(mod2_int4)

plot(effect(term="CpuClockSpeed:MemoryType", mod=mod2_int4))
plot(effect(term="CpuClockSpeed:MemoryType", mod=mod2_int4, default.levels=20), multiline=T) # same plot 
# There is some interaction between Hybrid and SSD in the plot. However, this interaction is not statistically significant. 

#----------- Model Diagnostics

#Checking for multicollinearity
vif(model2) 
sqrt(vif(model2))>2 

#There is some multicollinearity exhibited between the variables.
#Vif value around 6 for MemoryTypeHDD and MemoryTypeSSD : But they are levels of the same variable, hence, we can ignore it.


#Plotting the model and identifying multi-variate outliers.
par(mfrow=c(2,2))
plot(model2)

#Outliers : 907, 195, 1209, 972, 501

laptop1 <- laptop[-c(907, 195, 1209, 972, 501),]

#Rebuilding the model
model3<-lm(log(Price_euros) ~ TypeName+Cpu+CpuClockSpeed+Ram+MemoryType, data=laptop1)
summary(model3) 

vif(model3) #No Multicollinearity

dev.off()
par(mfrow=c(2,2))
plot(model3) #Diagnostic Plots look good.


#----------------------------- OLS Regression b/w Price & Origin------------------

#Performing an Anova test between Price & Origin
price_origin.aov <- aov(laptop$Price_euros ~laptop$Origin)
summary(price_origin.aov)
#p-value is 0.0564. We can say that there is statistical significance at 90% CI

model4<-lm(log(Price_euros) ~ Origin, data=laptop)
summary(model4) 
exp(coef(model4))

#p-value : 0.0052. There is definitely statistical significance at 99% CI.
#Adjusted R-squared is 0.52%

#The (log) price of a laptop of an American brand is 1.10 times more than that of an Asian Brand.

#----------------------------- Logit Regression b/w Origin & Brand Revenue ------------------

summary(laptop$BrandRevenue)
plot(density(laptop$BrandRevenue))

#Although, Brandrevenue is numeric, the values are discrete.
#Converting this into a factor as Revenue Less than 50B USD and Greater than 50B USD

laptop$brand1 <- as.factor(ifelse(laptop$BrandRevenue <= 50.64, "Less_than_50b", "Greater_than_50b"))
summary(laptop$brand1)

table(laptop$brand1,laptop$Origin)
prop.table(table(laptop$brand1,laptop$Origin)*100)

ggplot(laptop, aes(y=brand1, x=Origin, fill=brand1)) + 
  geom_bar( stat="identity")

#1.08% of Asian brands have revenue greater than 50B USD.
#1.08% of American brands have revenue less than 50B USD.

#Hypothesis : There is a relationship between brand revenue and origin.
#Null Hypothesis : There is no relationship between brand revenue and origin.
c1 <- chisq.test(laptop$brand1,laptop$Origin)
c1

#p-value is 2.2e-16. Hence, we accept our hypothesis.

mod1 <- glm(laptop$brand1~Origin, data = laptop, family = "binomial")
summary(mod1)
exp(coef(mod1))

mod_fit_1<- lrm(laptop$brand1~Origin, data = laptop)
mod_fit_1$stats["R2"] 

deviance(mod1)/df.residual(mod1) #0.208, No overdispersion

#p-value is 2e-16. Thus, there is a significant relationship between brand revenue and origin
#Pseudo adjusted r-squared is 92.25%. Origin explained 92.25% variance in Brand Revenue.
#The odds of an American Brand having revenue greater than $50GB is 2045 times higher than the odds of an Asian brand.

#----------------------------- Relation b/w Brand Revenue & Price ------------------

#Loaded the RData file named laptop_final.RData and created a new column called brand2 

summary(laptop$BrandRevenue)  
#creating new variable brand2 which is BrandRevenue as a factor with 3 levels 
laptop$brand2 <- as.factor(ifelse(laptop$BrandRevenue <= 30, "LOW-Asus,Xiaomi", 
                                  ifelse(laptop$BrandRevenue > 30 & laptop$BrandRevenue < 90, "MID-Dell,HP,Lenovo",
                                         "HIGH-Apple,Google,Microsoft")))

laptop$brand2 <- ordered(laptop$brand2, levels = c("LOW-Asus,Xiaomi", "MID-Dell,HP,Lenovo", "HIGH-Apple,Google,Microsoft"))
summary(laptop$brand2)

#LOW = ASUS, Mediacom, MSI, Razer, Xiaomi
#MID = Dell, Lenovo, Toshiba, HP, Huawei, Fujitsu
#HIGH = Apple, Microsoft, Google, LG, Samsung

#Performing a bivariate analysis --- Comparitive Boxplot, ANOVA test and OLS

#Plotting the log of Price and Brand Revenue
dev.off()
boxplot(log(Price_euros) ~ brand2, data=laptop, main="Price vs BrandRevenue",xlab="BrandRevenue", ylab="Price_euros", col=c("gold","darkorange2","firebrick"))
boxplot((Price_euros) ~ brand2, data=laptop, main="Price vs BrandRevenue",xlab="BrandRevenue", ylab="Price_euros", col=c("gold","darkorange2","firebrick"))
#Evidently, on an average, the price of laptops belonging to High level brands like Apple, Google, Microsoft have higher price 
# than those in lower and middle level brands such as Dell, Lenovo, Toshiba, HP, Huawei, Fujitsu etc. 


#Performing the ANOVA Test to reinforce the finding 
expense_catalog_model <- aov(log(Price_euros)~ brand2, data=laptop)
summary(expense_catalog_model) 
#Results show that the relationship between price and revenue is statistically significant 

#Performing OLS Regression to reinforce the finding 
model_1 <- lm(log(Price_euros) ~ brand2, data= laptop)
summary(model_1)
coeffs <- exp(model_1$coefficients)

#p-value : 0.0000001718
#Adjusted r-squared : 2.23%
#Results show that the relationship between price and revenue is statistically significant 

#Interpretation : The laptops belonging to higher level brands are 1.47 times more expensive than those in the lower level brands
# and The laptops belonging to medium level brands are 1.14 times more expensive compared to low level brands

# Business Value : The price of a laptop surges with the Brand name and impact of the company. 
# Investing towards building an ASPIRATIONAL BRAND can enable the company to charge their customers more

#----------------------------- Hypothesis b/w Origin & Specifications ------------------

#Performing Hypothesis tests.
##P-value is 0.006, origin and memory type have some relationship
chisq.test(laptop$Origin,laptop$MemoryType)
## Typename and RAM is significant
chisq.test(laptop$Origin,laptop$TypeName)
chisq.test(laptop$Origin,laptop$Ram)

##Memory is not significant
chisq.test(laptop$Origin,laptop$Memory)

str(laptop)
##Price is  significant
aov_price <- aov(log(laptop$Price_euros)~laptop$Origin, data=laptop)
summary(aov_price)

## ClockSpeed is not significant
aov_clockspeed <- aov(laptop$CpuClockSpeed~laptop$Origin, data=laptop)
summary(aov_clockspeed)

##Price is slightly significant
aov_weight <- aov(laptop$Weight~laptop$Origin, data=laptop)
summary(aov_weight)

##Screen Inches is slightly significant
aov_inches <- aov(laptop$Inches~laptop$Origin, data=laptop)
summary(aov_inches)

## We create the model with the significant factor variables
origin_mod1 <- glm(Origin~TypeName+Ram+Inches+Weight, data=laptop, family=binomial)
summary(origin_mod1)
##Inches was not significant, so we are removing from the model
## Also Weight is highly correlated with Screen size,
## Higher the screen size, higher the weight
origin_mod1_1 <- glm(Origin~TypeName+Ram+Weight, data=laptop, family=binomial)
summary(origin_mod1_1)

#Check for Multi Collinearity
vif(origin_mod1_1)
sqrt(vif(origin_mod1))>2 # There is multicollinearity in this modelwith Weight 

#Check for Over Dispersion
deviance(origin_mod1_1)/df.residual(origin_mod1_1)
## there may be Over dispersion, Value is slightly greater than 1

## We will check for Quasi Binomial Distribution
origin_mod1_od <- glm(Origin~TypeName+Ram+Weight, data=laptop,
                      family=quasibinomial) # note the quasibinomial distribution
pchisq(summary(origin_mod1_od)$dispersion * origin_mod1_od$df.residual,
       origin_mod1_od$df.residual, lower=F) 
##The value is 0.4587, there is no over dispersion


##Create model with all the significant numeric and factor variables
origin_mod2 <- glm(Origin~TypeName+Ram+Weight+Inches+log(Price_euros)+MemoryType, data=laptop, family=binomial)
summary(origin_mod2)
##AIC is 1711.3
##Weight and Inches is not significant

##Create new model removing the Weight and Inches
origin_mod2_1 <- glm(Origin~TypeName+Ram+log(Price_euros)+MemoryType, data=laptop, family=binomial)
summary(origin_mod2_1)
##AIC is 1711.1, Slight improvement

#Checking for multicollinearity
vif(origin_mod2_1)
sqrt(vif(origin_mod2_1))>2 
# There is no multicollinearity in this model! 

deviance(origin_mod2_1)/df.residual(origin_mod2_1)
## there is Over dispersion, Value is slightly greater than 1

##Check for over dispersion using quasibinomial
origin_mod2_od <- glm(Origin~TypeName+Ram+log(Price_euros)+MemoryType, data=laptop,
                      family=quasibinomial) # note the quasibinomial distribution

pchisq(summary(origin_mod2_od)$dispersion * origin_mod2_od$df.residual,
       origin_mod2_od$df.residual, lower=F) 
## The value is 0.4504, there is no over dispersion
## we reject the null hypothesis that there is overdispersion in the model

##Plotting the effects of Price and Memory Type
origin_mod4 <- glm(Origin~TypeName+Ram+Price_euros*MemoryType, data=laptop, family=binomial)
summary(origin_mod4) # not quite statistically significant, but it shows how this is done -- same as OLS! 
plot(effect(term="Price_euros*MemoryType", mod=origin_mod4, default.levels=20), multiline=T)
## there is no relationship between Inches and prices


origin_mod5 <- glm(Origin~TypeName+MemoryType+Ram*log(Price_euros), data=laptop, family=binomial)
summary(origin_mod5) # not quite statistically significant, but it shows how this is done -- same as OLS! 
plot(effect(term="Ram*log(Price_euros)", mod=origin_mod5, default.levels=20), multiline=T)
dev.off()

##Determining the accuracy of our model
accepted <- which(laptop$Origin=="USA") %>% head 
accepted
pred_origin <- predict(origin_mod2_1)
prob_origin <- 1/(1 + exp(-pred_origin))
summary(prob_origin)
predicted <- ifelse(prob_origin > 0.5, "USA", "ASIA") %>% as.factor
predicted[accepted]
conf <- confusionMatrix(table(laptop$Origin, predicted))
conf$overall[['Accuracy']]
conf
## Overall Accuracy is 60.7%

#The odds of a laptop being of an Asian Brand is higher if it has :
#Gaming / Netbook / Workstation
#More than 8 GB RAM
#HDD Memory Type
#(Log) price is 2.03

#2in1 Convertible and Notebook style laptops significantly dominated by American Brands. 


#----------------------------------- Exploratory Factor Analysis  -------------------------------

#Importing data

laptop13 <- read_csv("laptops.csv")
brand1 <- read_csv("laptops_brand.csv")
laptopfa <- merge(laptop12,brand1,by="Company")
#Removing Irrelevant columns
laptopfa$Rank <- NULL
laptopfa$BrandValue <- NULL
laptopfa$Country <- NULL
laptopfa$Continent <- NULL
laptopfa$`Laptopmag ranking` <- NULL


#Removing reows will null data
laptopfa <- laptopfa[-c(283,284,285,1296,1297,1298,1299),] 

glimpse(laptopfa)


#We will keep all the numeric variables as numeric, instead of converting them to factors.
#The variables are: Inches, Ram, Memory, Weight, BrandRevenue, CpuClockSpeed

#----------------------------------- Correcting datatypes  -------------------------------
#Dealing with numeric vars first.
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

#Extracting only digits from these variables and converting them to numeric data type
laptopfa$Ram <- numextract(laptopfa$Ram)
laptopfa$Ram <- as.numeric(laptopfa$Ram)

laptopfa$CpuClockSpeed <- word(laptopfa$Cpu,-1)
laptopfa$CpuClockSpeed <- numextract(laptopfa$CpuClockSpeed)
laptopfa$CpuClockSpeed <- as.numeric(laptopfa$CpuClockSpeed)

laptopfa$Memory <- numextract(laptopfa$Memory)
laptopfa$Memory <- as.numeric(laptopfa$Memory)
laptopfa$Memory[which(laptopfa$Memory==1)] = 1000 #Replacing 1TB will 1000GB
laptopfa$Memory[which(laptopfa$Memory==2)] = 2000 #Replacing 2TB will 2000GB

laptopfa$Weight <- numextract(laptopfa$Weight)
laptopfa$Weight <- as.numeric(laptopfa$Weight)

laptopfa$Price_euros <- as.numeric(laptopfa$Price_euros)


#Factor vars

laptopfa$Company <- as.factor(laptopfa$Company)
laptopfa$TypeName <- as.factor(laptopfa$TypeName)

#Extracting only resolution values from "ScreenResolution" column
laptopfa$ScreenResolution <- word(laptopfa$ScreenResolution,-1)
laptopfa$ScreenResolution <- as.factor(laptopfa$ScreenResolution)

#Splitting Cpu column into 2 : Cpu and CpuClockSpeed.
laptopfa$Cpu <- word(laptopfa$Cpu, 1, sep=" ")
laptopfa$Cpu <- as.factor(laptopfa$Cpu)

laptopfa$OpSys <- as.factor(laptopfa$OpSys)


#Extracting only Gpu Company Name from Gpu column.
laptopfa$Gpu <- word(laptopfa$Gpu, 1, sep=" ")
laptopfa$Gpu <- as.factor(laptopfa$Gpu)

glimpse(laptopfa)

#-------------------------------- Factor Analysis ------------------------------
Filter(is.numeric, laptopfa)

# keep only the numeric vars. Price_euros not included as it is DV
laptopfa_s <- data.frame(scale(laptopfa[,c(5,8,9,12,14,15)]))

sapply(laptopfa_s, FUN=mean) %>% round(4)
sapply(laptopfa_s, FUN=sd) %>% round(4) #looks good

#Drawing a correlation plot
corrplot(cor(laptopfa_s), method="circle", addCoef.col="grey", type="upper") 

#We can clearly see a correlation between the foll. variables :
#1. Weight and Inches

#Parallel Analysis

fa.parallel(laptopfa_s, fa="fa", n.iter=100, show.legend=T) 
#Parallel analysis suggests that the number of factors =  3 

laptopfafa1 <- factanal(laptopfa_s, 3) 
laptopfafa1$loadings
dev.off()

#Factor Analysis suggests the foll. factors :-
# Factor 1: Ram (CPU and Weight can be taken intro consideration)
# Factor 2 : Inches, Weight
# Factor 3 : Weight (inches can be taken into consideration)


#-------------------------------- Saving Environment into Rdata file ------------------------------

save.image(file="IDS_462_Final_Project.RData")
