# ---------------------------------------------------------------------------- #
#   Author - Anupama Rajaram
#   Program - ANOVA  
#   Dataset - nesarc.csv
# ---------------------------------------------------------------------------- #

# prepping the environment - clean up memory, set number formatting
rm(list=ls(all=TRUE))
options(digits=2)

data = read.delim(file = 'nesarc.csv', header = TRUE, sep = ',', dec = '.')

# =========================================================================== #
#   ANOVA
# =========================================================================== #
vars <- c("WEIGHT", "AGE", "SEX")
summary(data[vars])


# One Way Anova - Randomized Design
fit <- aov(ETHRACE2A ~ SEX, data=data)
fit
plot(fit)  # this gives 4 graphs. a) Residuals vs Fitted. (b) Normal Q-Q 
      # (C) Scale vs Location. (d) Residuals vs Leverage

summary(fit)  # gives p-val, F-value etc.

# Analysis of Covariance 
fit2 <- aov(ETHRACE2A ~ SEX + REGION, data=data)
fit2 
summary(fit2)

interaction.plot(data$ETHRACE2A, data$SEX, data$REGION, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),	
                 )  # for visualization
