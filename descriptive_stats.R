# ---------------------------------------------------------------------------- #
#   Author - Anupama Rajaram
#   Program - Basic Descriptive and Inferential statistics for 
#             data exploration. 
#             a. Measures of location & scale for quantitative variables.
#             b. Frequency & contingency tables (chi-square tests) for 
#                 categorical variables. 
#             c. correlation coefficients for continuous, ordinal variables. 
# ---------------------------------------------------------------------------- #

# prepping the environment - clean up memory, set number formatting
rm(list=ls(all=TRUE))
options(digits=2)

data = read.delim(file = 'nesarc.csv', header = TRUE, sep = ',', dec = '.')

# =========================================================================== #
#   Functions for descriptive statistics
# =========================================================================== #
vars <- c("WEIGHT", "AGE", "SEX")
summary(data[vars])

# function = sapply(), with mean, sd, var, min, max, median, length, range, 
# and quantile. Also fivenum(), which returns Tukey’s five-number summary 
# (minimum, lower-hinge, median, upper-hinge and maximum).
sapply(data[, 1:30],function(x) sum(is.na(x)))  # counts the number of NAs or empty 
                                            # records for each specified column.

# create custom function
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}

sapply(data[,1:10], mystats) # apply custom function to first 10 columns ONLY


# custom function 2
stat_fun <- function(x){
    mean <- mean(x)
  med <- median(x)
  min1 <- min(x)
  max2 <- max(x)
  len <- length(x)
  ran1 <- range(x)
  q1 <- quantile(x)
  f1 <- fivenum(x)
  return(c(mean= mean, median = med, min = min1, max = max2, length = len, 
           range = ran1, quantile = q1, fivenum= f1 ))
}

sapply(data[vars], stat_fun)  # apply custom funtion to 3 specific columns

# there are other complex functions for descriptive statistics under the packages:
# Hmisc, pastecs, and psych. "pastecs" package has a nice function called stat.desc()

# aggregate fucntion can be used to explore data based on moderator variables
aggregate(data[vars], by=list(race=data$ETHRACE2A), mean)  
    # ETHR.. is used here as moderator variable

# freqency tables 
 ft1 <- with(data, table(ETHRACE2A)) # Frequency as COUNT
 ft1
 
 ft2 <- prop.table(table(data$ETHRACE2A))  # Frequency as proportion
 ft2 
 
 ft3 <- prop.table(table(data$ETHRACE2A))*100  # Frequency as percentage
 ft3
 
 
 # 2-way frequency tables, this can hold more than 2 variables.
 tab1 <- xtabs(~ ETHRACE2A + SEX, data=data)  #SEX : 1 = MALE, 2 = FEMALE
 tab1
 
 tab2 <- xtabs(~ ETHRACE2A + SEX + REGION, data=data) 
 tab2
 
 # CrossTable() function (package = gmodels) creates a highlt formatted 
 # two-way tables similar to the 2-way tables using PROC FREQ in SAS 
 # or CROSSTABS in SPSS.
   CrossTable(data$ETHRACE2A, data$SEX)
   
   
