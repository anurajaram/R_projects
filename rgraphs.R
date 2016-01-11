# __________________________________________________________________
# //////////////////////////////////////////////////////////////////
#
# Author - Anupama Rajaram
# 
# Program Description - Different types of graphs in R
#
# Dataset - 
#   (1) boston data from package = "MASS"
#   (2) w2_salary_data.csv. This dataset comes from company called 
#     Dognition, and holds info about its canine members. The data was 
#     compiled and provided by Duke Univ as part of the Tableau course on
#     the Coursera platform. dataset can be found at location 
#     
#
# Note: anything commented as 'Data Exploration' is purely for 
#       debugging purposes, and can be deleted without affecting this
#       script.
# __________________________________________________________________
# //////////////////////////////////////////////////////////////////


# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))


#===================================================================#
#=========== Section 1: Data Management ============================#

# Load text file into local variable called 'data' - 167278 rows and 26 columns
b = Boston

# using dognition dataset
sal = read.delim(file = 'dognition_data.csv', header = TRUE, 
                 sep = ',', dec = '.')
sal1 <- sal[1:200,]
sal1$bday <- as.integer(format(sal1$bday))  # converting bday to integer to indicate year of birth


attach(b)  # to avoid having to call the dataframe repeatedly
plot(age, crim, main = "graph1", xlab = "Crime rate", ylab = "age")

abline(lm(crim~age), col = "red")  # Add fit linesfor regression
lines(lowess(age,crim), col="blue") # lowess line (x,y)

# Enhanced Scatterplot of MPG vs. Weight 
# by Number of Car Cylinders 
library(car) 

attach(mtcars)
scatterplot(mpg ~ wt , data=mtcars, 
            xlab="Weight of Car", ylab="Miles Per Gallon", 
            main="Enhanced Scatter Plot"            )

# ---------- histogram ------------- #
hist(b$age,  col = "blue", main = "age distribution",
     xlab = "age")  # view simple histogram


# ---------- bar plots ------------- #
counts <- table(b$ptratio)
barplot(counts, main="bar plot chart1", 
        xlab="ratio", ylab = "count")  # formula for barplot

# ---------- box plots ------------- #
boxplot(medv~ptratio,data=b, main="Crime ptratio chart", 
        xlab="ptratio", ylab="crime")

# ------------ Simple Pie Chart -------------- #
r <- prop.table(table(sal$breed_group))
r<- (r*100)  # to convert to %
slices <- r
lbls <- sal1$breed_group
pie(slices, labels = lbls, main="Pie Chart of breed type" )


# ------------ Kernel Density Plot -------------#
d <- density(sal1$bday) # returns the density data 
plot(d, main = "Number of dogs based on Birthyear",
     xlab = "Birth Year" , ylab =  "Density") # plots the results
