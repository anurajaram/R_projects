# ---------------------------------------------------------------------------- #
#   Author - Anupama Rajaram
#   Program - Linear and multiple Regression  
# ---------------------------------------------------------------------------- #

 # prepping the environment - clean up memory, set number formatting
 rm(list=ls(all=TRUE))
 options(digits=2)
 
 library(car)
 
 titanic = read.delim(file = 'train.csv', header = TRUE, sep = ',', dec = '.')
 attach(titanic)
 
 # basic linear regression
 regr <- lm(crim ~. , data = Boston)
 summary(regr)
 
 # multiple linear regression
 lr1 <- lm(Survived ~.-Name -Cabin -Ticket, data = titanic)
 summary(lr1)  # this will clearly show the most significant explanatory variables
 plot(lr1)
 anova(lr1)
 AIC(lr1)  # Akaike’s Information Criterion
 
 coefficients(lr1) # gives you the coeff of the regression equation
 confint(lr1) # to compare if the significance is actually true.
 
 # visulaization 
 scatterplotMatrix(titanic, spread=FALSE, lty.smooth=2,
                   main="Scatter Plot Matrix")
 
 qqPlot(lr1, labels=row.names(titanic), id.method="identify",
        simulate=TRUE, main="Q-Q Plot")  # generates interactive probability plot 
 
 outlierTest(lr1)  # check for outliers
 
 hat.plot <- function(lr1) {
   p <- length(coefficients(lr1))
   n <- length(fitted(lr1))
   plot(hatvalues(lr1), main="Index Plot of Hat Values")
   abline(h=c(2,3)*p/n, col="red", lty=2)
   identify(1:n, hatvalues(lr1), names(hatvalues(lr1)))
 }
 hat.plot(lr1)  # to check for influential observations
 
