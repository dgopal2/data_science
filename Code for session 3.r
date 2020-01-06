#=======================================
# IDS 462, Session 3
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#=======================================

## Load libraries and data 
#= 
library(tidyverse)
library(lubridate)
library(stringr)

# Getting data
#= 
# Building permits from the City of Chicago: https://data.cityofchicago.org/Buildings/Building-Permits/ydr8-5enu/data

building_permits <- read_csv("Building_Permits.csv") # read_csv is much faster compared to read.csv. 

# Random sample 
#=
# The building permit data file is too large. 
# For efficiency, we are going to work on a smaller random sample. 
# We then can apply the code to the initial (full) data 

# Using R base 
numrows <- nrow(building_permits)
sampsize <- round(numrows*0.05,0)
set.seed(462) 
samp <- sample(numrows, sampsize) 
bp <- building_permits[samp,]
dim(bp) 

# Using tidyverse 
set.seed(462)
bp <- sample_frac(building_permits, 0.05) # this function takes a random sample equals to a percentage
# sample_n samples using a perset number of cases 


# Missing data 
#= 

# Start by examining the data. There are often columns with many missing cases. 

# NA, without quotations, is the indicator for missing data 

# The is.na function returns logical vector to identify missing values 

is.na(bp$PERMIT_TYPE)
table(is.na(bp$PERMIT_TYPE))

 # Let's try another one 
table(is.na(bp$ISSUE_DATE)) 

# You get the idea. 
# The problem is that we have 131 columns! 
# This number of columns is typical for real data. 
# Running the is.na function per column is not particularly efficient. 
# Here is a better approach: 

colSums(is.na(bp)) # colSums sums up the values from a numeric vector/column  

View(colSums(is.na(bp))) 

# It is preferable to view NAs in percentage

missing <- round(colSums(is.na(bp))/nrow(bp),2)

View(missing) 

# What if you want the opposite, how many rows HAVE data? 

notmissing <- round(colSums(!is.na(bp))/nrow(bp),2)
View(notmissing) 
	
# Drop selected columns with NAs
# We know that we can't do much with columns with too many missing values 

keep <- missing[missing < 0.4]
keep <- names(keep) 
bp1 <- bp[,keep]
bp1

# Examine NAs for patterns 
# the easiest way is to consider NAs by specific columns of interest, especially the DV 

summary(bp[is.na(bp$CONTRACTOR_1_PHONE), c("ESTIMATED_COST","AMOUNT_PAID")]) # oops! 
str(bp1)
# we need to convert these rows to numeric 
# you already know how to convert one column at a time. 
# here is one way to convert multiple columns to the same type. 
	
convert <- c("ESTIMATED_COST","AMOUNT_PAID")
pb[convert] <- sapply(bp[convert],as.numeric) # not good enough #sapply loop function

head(bp$ESTIMATED_COST); head(bp$AMOUNT_PAID) # that's why! 

# String manipulation will help addressing this problem 

	
# NOTES
# For the most part, the tidyverse approach to getting percentage missing values per column is cumbursome. 
# See options at: https://stackoverflow.com/questions/41819422/how-to-drop-columns-by-passing-variable-name-with-dplyr#41819540
# To drop columns with tidyverse: 
bp %>% select(-c("PIN1", "PIN2"))

####################
# Go to Data.gov and find a CSV data set with at least 200 rows and 10 columns
# If your data is very large, take a smaller random sample. 
# Your data likely has missing values in different columns 
# Examine all the data frame for missing values
# What is the average (mean) missing values in the data frame? 
# How many columns have more than 20% missing values? 
# Save all the columns that have 80% or more values as a separate data frame
####################

# Manipulating strings (characters) 
#= 
# We use a common procedure in computing languages, called regular expression 
# It offers a simple yet powerful way to handle messy data. 
# It is used to find, extract, replace, modify, and add characters. 
# Other uses include splitting and gathering data. 

# Match patterns 

# R base 
grep("$", cost) # row output 
grep("$", cost, value=T) # value output 
grepl("$", cost) # logical output 
grep("$50", cost)
grep("\\$50", cost) # escape special symbols, such as $,.() etc. 
grep("\\$50|\\$20", cost) # the | stands for "or" 
grep("\\b500\\b", cost, value=T) # \\b indicates boundaries

# Tidyverse 
# No good alternative to grep, however: 
cost %>% str_detect("\\$50") # same as grepl

# Finding location of pattern 
cost %>% str_locate("\\$50") # R base equivalent is gregexpr

# Replace patterns 

# R base 
digitpattern <- "\\$|\\.00|\\,"
gsub(digitpattern, "", cost) # remember to reassign the processed output and convert to the correct data type if needed 
bp$ESTIMATED_COST <- as.numeric(gsub(digitpattern, "", bp$ESTIMATED_COST))

# Tidyverse 
cost %>% str_replace(digitpattern, "") %>% as.numeric() %>% head()

# Extracting a pattern 

# R base 
nums <- "\\d{1,}" # Extract only digits that follow one another 
regmatches(cost, regexpr(nums, cost))

# Tidyverse 
# Much easier! 
cost %>% str_extract(nums) 

####################
# Copy several columns that contain missing values in your data to another name 
# Change missing values in two columns in your data to "Missing" 
# Change all other values in these columns to "NotMissing"  
# Remove odd characters in the data, such as commas 
# and other non-numeric symbols in numeric variables 
####################

# Reshaping data with string patterns 
#= 

# Splitting a column 

# First test the pattern 

issue <- bp1$ISSUE_DATE[1:20]


# R base 
strsplit(issue, "/") 
splitissue <- strsplit(issue, "/") 
class(splitissue)
sapply(splitissue, "[", 1) # month "column"
sapply(splitissue, "[", 3) # year "column"

# Tidyverse
bp1 %>% separate(ISSUE_DATE, c("month", "day", "year"), sep="/") %>% select(ISSUE_DATE, month, day, year) %>% head()  # oy! 

bp1 %>% separate(ISSUE_DATE, c("month", "day", "year"), sep="/", remove=F) %>% select(ISSUE_DATE, month, day, year) %>% head() 

# Combining multiple columns 

# R base 
paste(bp$STREET_NUMBER, bp$STREET_DIRECTION, bp$STREET_NAME, bp$SUFFIX, sep=" ") %>% head()  
# doesn't work because there are spaces in the column name "STREET DIRECTION"! let's fix this 
paste(bp$STREET_NUMBER, bp$"STREET DIRECTION", bp$STREET_NAME, bp$SUFFIX, sep=" ") %>% head()  
# create a new column 
bp1$address <- paste(bp$STREET_NUMBER, bp$"STREET DIRECTION", bp$STREET_NAME, bp$SUFFIX, sep=" ")   

# Tidyverse
bp1 <- bp1 %>% unite(address1, c("STREET_NUMBER","STREET DIRECTION","STREET_NAME"), sep=" ")
head(bp1$address); head(bp1$address1)

#################
# Separate your date column(s) into year and months. 
# And/or select other columns that you think should be separated for analysis 
# Combine back the above and/or other relevant columns into one 
#################

# Date (and time) data 
#= 

# R's time objects are based on UNIX chron. This means dates and times (seconds), since 1970. 
# Time zone needs to be provided. 

issue %>% class() 

# R base 

as.Date(issue) # nope 
as.Date(issue, format="%m/%d/%Y") %>% class #Y is for four digit year. y is for two. 
	# ?strptime for a comprehensive list. 
date1 <- as.Date(issue, format="%m/%d/%Y") 

# Lubridate
# First, understand date format, in this case, month, day, year. 

mdy(issue) %>% print() %>% class() 

# Higher frequency time data  

# For higher frequency times, it is best to use POSIX
sometimes <- c("1/1/2017 7:00:01", "1/2/2017 7:00:20", "1/3/2017 7:02:15")

as.POSIXct(sometimes, format="%m/%d/%Y %H:%M:%S") # POSIXct (calendar time) based on seconds since UNIX epoch; POSIXlt (local time). 

# Time zone 
# Default is local machine. 
Sys.timezone() 
# To input a specific time zone: 
as.POSIXct(sometimes, format="%m/%d/%Y %H:%M:%S", tz="Africa/Abidjan")

# Lubridate 
times <- mdy_hms(sometimes, tz="Africa/Abidjan") %>% print() 
# NOTES 
# For a list of timezones: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones  

# Extracting time components (year, month, etc.)
#= 

# R base 

# We simply use regex 
format(times, "%Y")
format(times, "%y")
format(times, "%B")
weekdays(times)
months(times)
quarters(times)
format(times, "%S")

# Lubridate 

year(times)
yday(times)
minute(times)
second(times)

# Applied to the building permit data 
bp1$issue_date <- mdy(bp1$ISSUE_DATE)

summary(bp1$issue_date) # looks like we have NAs

####################
# Using R base, convert the date columns in your data to a date class
# Using lubridate, convert the date columns in your data to a date class
# What is the median date? 
# Extract quarter into a new column 
# Extract year into a new column 
# Combine quarter and year into a new column 
####################


############### 
# Additional resources: 
# Learning R, ch 13 
# Boehmke, Bradley. 2016. Data Wrangling with R , 1st ed. Springer.
# http://vita.had.co.nz/papers/tidy-data.pdf
# https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
