#=======================================
# IDS 462, Session 1 
# Copyright Zack Kertcher, PhD, 2018. All rights reserved. 
# Do not distribute or use outside this class without explicit permission from the instructor.  
#=======================================

# First steps in R  
#====

# Orientation 
#=
getwd() # shows us where the working directory is 
dir.create('IDS462') # creates a directory called IDS462. Note that R is, for the most part, accpets both single and double quatation marks 
setwd("IDS462")
getwd() 

# Install a package 
#= 
# use RStudio's gui, or 
install.packages("dplyr")
library(dplyr) # makes all functions in dplyr available, or 
dplyr::  # brings up a list of functions to choose from 

#=====================
# Create a folder called IDS462 under a temp directory (create it if you need to) 
# In RStudio, change your working directory to the newly created directory 
# Install the tidyverse package
#=====================

## Solution
# you can create a folder by using your computer's operating system, or through R: 

dir.create("C:/temp/IDS462") 
setwd("C:/temp/IDS462")
install.packages('tidyverse')

# Basic operations 
#=
5+6
8*7
10*5 / 0.5

# Vectors  
somet

x <- "IDS462"
y <- "is where you will learn that"
z <- "equals"
something <- c(x,y, a, "plus", b, z, c) # c is a function, stands for combine, or concatenate 
something 
length(something) # length is also a function 
something; length(something)

#=====================
# Assign the numbers 4, 3.7, 4, 3.5 to object score
# Assign the number 4 to object tests
# Assign the result of score divided by tests to object avg.score
# Print the content of avg.score 
# Assign the number 4 to object score
# Did avg.score change? Why?
#=====================

## Solution
score <- c(4,3.7,4,3.5)
tests <- 4
avg.score <- score/tests
avg.score
score <- 4
avg.score
# It's content did not change because the object avg.score was unchanged 
# if u say avg.score <- score/tests again then the result ll be 1. need to define avg.score again

# Data types
#= 
is.vector(something)
is.vector(something)==FALSE
class(something) # class is also a function
class(c)
class(a>b)
a>b
?class # help on a function 
help(package="ggplot2") # help on a package (along with its functions) 

# Classes
#=
a <-5
b<-"hello"
c<-FALSE 

class(a)
class(b)
class(c)
is.logical(a)
is.numeric(a)==FALSE
class(a)!=class(c)

#=====================================
# What is the data class of avg.score?
# Add the value "employee" to avg.score like this: avg.score <- c("employee", avg.score)
# Print the content of avg.score
# What is the class of avg.score now? Why is it different? 
#=====================================

## Solution
class(avg.score)
# [1] "numeric"
avg.score <- c("employee", avg.score)
avg.score
class(avg.score)
# [1] "character"
# The data type has changed  because we added a character value to avg.score 


#. Functions (note that R is a functional prog. language)
a1<-1:10
a2<-seq(1:10)
identical(a1,a2)
?identical
?seq()
a3 <- seq(from=1, to=10, by=0.01)
a3 # oops. don't forget to assign! 
a3 <- round(a3,1)
a3
head(a3)
tail(a3)

#=====================
# Generate a sequence of numbers from 7 to 100, by 4.05, and assign it to my.seq
# View the last 4 numbers like this: tail(my.seq,n=4)
# Round the last four numbers of my.seq to a single decimal, and assign them to new.seq
#=====================

## Solution
my.seq <- seq(from=7, to=100, by=4.05)
new.seq <- round(tail(my.seq,n=4),1)

# Environment 
#= 
ls()  # shows a list of objects that were saved into the R environment (also see them in the top-right pane)
save(a,b,c, file="firststeps.Rdata")
list.files()
rm(list=ls())
ls()
load("firststeps.Rdata")
ls()



