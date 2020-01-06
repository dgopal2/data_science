#=======================================
# IDS 462, Session 2
# Copyright Zack Kertcher, PhD, 2017. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#=======================================

#### WORKING WITH DATA 

## Load libraries 
library(tidyverse)
library(lubridate) 
## Business Challenge: 
#  A supply firm wants to target with coupons and other 
# incentives clients who are 1 standard deviation or more below the 
# mean. 

# Below is a small data subset 

#ID	Plumbing	HVAC	Electric	Member	StartDate
#2045	10040	238	63	0	5/4/2012
#2046	12000	248	55	0	6/1/2008
#2047	8240	201	45	1	12/9/2015
#2048	7160	206	38	0	4/25/2009
#2049	9900	188	50	1	5/19/2016
#2050	10240	213	70	1	9/1/2010
#2051	8200	201	38	0	6/12/2014
#2052	12500	238	75	0	9/15/2015
#2053	11460	223	68	0	1/30/2009
#2054	10440	216	45	0	6/23/2012

# Inputing the data into R 
#==
 
# Method 1: Using vectors 
ID <- c(2045,2046,2047,  2048, 2049,2050,2051,2052,2053,2054) # Note that R is *not* sensitive to spaces
Plumbing <- c(10040,12000,8240,7160,9900,10240,8200,12500,11460,10440)
HVAC <- c(238,248,201,206,188,213,201,238,223,216)
Electric <- c(63,55,45,38,50,70,38,75,68,45)
Member <- c(0,0,1,0,1,1,0,0,0,0)
StartDate<-c("5/4/2012","6/1/2008","12/9/2015"
             ,"4/25/2009","5/19/2016","9/1/2010","6/12/2014"
             ,"9/15/2015","1/30/2009","6/23/2012")
  # If vectors are the same length, you may combine them into a data.frame
  # check if vectors are the same length: 
length(ID); length(Plumbing) # ... Yes! all have 10 elements 
  # Combine into a data frame 
supply1 <- data.frame(ID, Plumbing, HVAC, Electric, Member, StartDate)
  # Alternatively, I can combine the columns using cbind 
supply2 <- cbind(ID, Plumbing, HVAC, Electric, Member, StartDate)
  
class(supply1); class(supply2) # Interesting

supply2 <- as.data.frame(supply2)

class(supply1); class(supply2)  # Good! 
  
# Method 2: Input all data directly  
supplydatainput <- data.frame(
  ID=c(2045,2046,2047,  2048, 2049,2050,2051,2052,2053,2054), 
  Plumbing=c(10040,12000,8240,7160,9900,10240,8200,12500,11460,10440),
  HVAC=c(238,248,201,206,188,213,201,238,223,216),
  Electric=c(63,55,45,38,50,70,38,75,68,45),
  Member=c(0,0,1,0,1,1,0,0,0,0),
  StartDate=c("5/4/2012","6/1/2008","12/9/2015","4/25/2009","5/19/2016",
              "9/1/2010","6/12/2014","9/15/2015","1/30/2009","6/23/2012")
    )

# Method 3: Read from clipboard 
# First, copy content 
# Then, run the following: 

supplyclip <- read.delim("clipboard")  

# Method 4: Read from file 

supplyfile <- read.csv("supplysamp.csv", header=T) # Or specify the full path, e.g., "/home/data/supplysamp.csv"
  # Also, try using file.choose()

# Method 5: Read using tidyverse (readr)  

supplytibble <- read_csv("C:/Users/DilipKumar/Desktop/462 R/supplysamp.csv") 

class(supplytibble) 
is.data.frame(supplytibble)
is.tibble(supplytibble)

# We can also convert a dataframe to a tibble, using the function: tibble()
# using data.frame() will convert to a data frame. 

#=============================
# (1) Download and open the employees.csv file from Blackboard (e.g., Excel, Google Sheets)
# (2) Use Method 1 above (using vectors) to recreate the data 
# Save as a data frame called EmpManual
# (3) Load the csv file directly into R as a tibble called employees
#=============================

# Solution 
#=============================
employee <- c('AA','BJ','FS', 'MS', 'NR', 'RW')
gender <- c(0, 1, 0, 1, 0, 1)
salarykd <- c(6.1,8.3,7,122.7,88,100.8)
months <- c(87,2,14,43,18,5)
EmpManual <- data.frame(employee, gender, months, salarykd)
employees <- read_csv("C:/Users/DilipKumar/Downloads/employees.csv") 

## Data Types 
#=
# Start by examining each column and note expected data type 

# We have 4 common data types in R: 
  # Numeric (commonly, integer and double -- for precision)
  # Character (strings)
  # Factor (also numeric, but with distinct levels)
  # Date (character, but is recognized as having date properties)


# What are the data types in our data? 

supplyfile 

# Look at each column more closely 
str(supplyfile)
str(supplytibble) 


# Alternatively, using tidyverse 
glimpse(supplyfile) 


# Do not worry about the difference between integer (whole numbers) and double (decimals, double-precision floating point number) 
class(supplyfile$ID)
is.numeric(supplyfile$ID)

# Change columns to their correct type 
supplyfile$ID <- as.character(supplyfile$ID)
supplyfile$Member <- as.factor(supplyfile$Member)
supplyfile$StartDate <- as.character(supplyfile$StartDate) # more on dates shortly 

glimpse(supplyfile)

# Examine data in a spreadsheet-like format 
View(supplyfile)

# All looks fine, except StartDate. It is a date variable but it is currently a character. 
# We will learn more about handling date variables, but let's take a shortcut 
  # Inspect date format 
head(supplyfile$StartDate) # Look for the format! This one is in m/d/y format
  # Use lubridate (loaded with tidyverse) to convert the character into a date 
supplyfile$StartDate <- mdy(supplyfile$StartDate)
glimpse(supplyfile)
summary(supplyfile$StartDate) # Nice! 

#=============================
# (1) Examine the variables (columns) in the employees data frame/tibble 
# (2) Think what data type each one should be
# (3) If needed, convert to the correct data type 
#=============================

# Solution 
#=============================

# Gender needs to change to a factor. All other columns are fine. 
employees$gender_factor <- as.factor(employees$gender)
glimpse(employees) 
# Also notice: 
class(employees$gender) # but 
typeof(employees$gender)

## Sorting Data 

# Method 1: Base R (order) 

supplyfile[order(supplyfile$Plumbing),] # default is ascending order 
supplyfile[order(-supplyfile$Plumbing),] # use negative sign for descending order 

supplyfile <- supplyfile[order(-supplyfile$Plumbing, # first, descending order of Plumbing
              Member),] # then by member 

# Method 2: Tidyverse (dplyr)
supplyfile %>% arrange(desc(HVAC), Plumbing)

	# If you are getting warning messages, ignore them. Or ask R to suppress them: options(warn=-1); options(warn=0) returns to default. 

#=============================
# (1) Sort the employees data frame by salary (descending) 
# and gender (ascending)
# (2) Using methods 1 (based R) and 2 (tidyverse) 
##=============================

# Solution 
#=============================
employees[order(-employees$salarykd, employees$gender),] 
employees %>% arrange(desc(salarykd), gender)


## Selecting (subsetting) Data 
# We now want to "extract" or subset those customers who are 1 sd below others 
# First, consider how to subset data in R, with a couple simple examples 
# Let's start with subsetting all customers who are members 
# then selecting all customers who are purchasing 1sd below the mean 

# Method 1: Using R Base
supplyfile$Member==1 # This is a logical statement that evaluates if membership equals 1  
# What we now need it subset all the TRUEs 
supplyfile[supplyfile$Member==1,] # From supplyfile df, select rows that are Member==1 (TRUE), and subset all columns. 
  # If we also want to select columns 
  supplyfile[supplyfile$Member==1, c("ID","Member", "Plumbing")] # Note change in structure! 
  # If we want to select rows and columns by position: 
  supplyfile[c(1:3), c(2:4)]

# Remember to assign the result to a new object: 
supplymembers <- supplyfile[supplyfile$Member==1,]

# Now for the 1sd below the mean 
# First, we will generate a variable called total_purchase 
supplyfile$total_purchase <- supplyfile$Plumbing+supplyfile$HVAC+supplyfile$Electric
glimpse(supplyfile)
# Next, compute threshold 
summary(supplyfile$total_purchase)
mean(supplyfile$total_purchase); sd(supplyfile$total_purchase)
threshold <- mean(supplyfile$total_purchase)-sd(supplyfile$total_purchase)
# Finally, select target customers 
target <- supplyfile[supplyfile$total_purchase<=threshold,]

# Method 2: Using the subset function in base R 

target1 <- subset(supplyfile, total_purchase<=threshold, # selects rows 
                  select=c("Member", "total_purchase")) # selects columns 

# Method 3: Using tidyverse 

target2 <- supplyfile %>% filter(total_purchase<=threshold) %>% select(StartDate, Member, total_purchase) # selects columns 

#=============================
# (1) Subset the rows with male employees
# then compute the highest salary for this gender
# (2) Repeat the above with female employees 
# (3) Subset the gender and salary of employees who are in the
# company less than the average number of months 
#
# Brainstorm as many ways you can think of! 
#=============================

# Solution 
#=============================
empmale <- employees[employees$gender_factor==1,]
MinSalary_Male <- min(empmale$salarykd)

MinSalary_Female <- employees %>% filter(gender_factor==0) %>% select(salarykd) %>% min() 

avgmon <- mean(employees$months)
LessAvgMon <- employees %>% filter(months < avgmon) %>% select(gender_factor, salarykd)

## Data values conversion 
# We'll start with a simple conversion known as recoding. 
# Suppose that instead of 0,1 for membership we want to change to meaningful labels 
# 0=NotMember, 1=Member

# Method 1: Using R base (simple assignment)
supplyfile$MemberFactor[supplyfile$Member==1]<-"Member"
supplyfile$MemberFactor[supplyfile$Member==0]<-"NotMember"
	# Note the structure: NewVariableName[DataFrame$ColumnName == (or any other condition) value ] <- "NewLabel" (can also be a number, TRUE/FALSE, etc. 


# Method 2: Using tidyverse (dplyr)
supplyfile$MemberFactor1 <-  recode_factor(supplyfile$Member, "0"="NotMember", "1"="Member")
  # There is also the fct_recode from the forcats package, however, it is still a bit buggy. 

# Now let's recode our customers as "Target" and "NonTarget"
# (1 sd below the mean are the Target). 

# Method 1: Assign values 
supplyfile$target[supplyfile$total_purchase<=threshold]<-"Target"
supplyfile$target[supplyfile$total_purchase>threshold]<-"NonTarget"
table(supplyfile$target)

# Method 2: ifelse 
ifelse(supplyfile$total_purchase<=threshold, "Target", "NonTarget") %>% table()
  

## Extra: Consider variable distribution 
#=
# Before manipulating data, you should check the distribution of the variables you are working with 
# For example, you would need to scale or perform other mathematical transformations 
# before subsetting and recoding values 
# We will work more on this in descriptive analytics, but meanwhile: 

  # Let's run a histogram and a boxplot 
hist(supplyfile$Plumbing)
boxplot(supplyfile$Plumbing)
  # Actually, let's plot them side-by-side 
par(mfrow=c(2,1))
hist(supplyfile$Plumbing)
boxplot(supplyfile$Plumbing)
  # Now the other three variables 
par(mfrow=c(2,2))
hist(supplyfile$Plumbing)
hist(supplyfile$Electric)
hist(supplyfile$HVAC)
hist(supplyfile$total_purchase)

boxplot(supplyfile$Plumbing)
boxplot(supplyfile$Electric)
boxplot(supplyfile$HVAC)
boxplot(supplyfile$total_purchase)


############### 
# Additional resources: 
# swirl: modules: 6, 7, and 12 
# Learning R: ch. 4 (only vectors), ch. 5 (data frames), ch. 7 
# Practice the above using other data sets. 
# Find a list of data sets here: https://vincentarelbundock.github.io/Rdatasets/datasets.html



