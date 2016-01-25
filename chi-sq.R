# ---------------------------------------------------------------------------- #
#   Author - Anupama Rajaram
#   Program - chi-square & similar tests for categorical variables. 
#             visualize using mosaic function
# ---------------------------------------------------------------------------- #

# prepping the environment - clean up memory, set number formatting, load data
rm(list=ls(all=TRUE))
options(digits=2)
library("vcd")
data = read.delim(file = 'nesarc.csv', header = TRUE, sep = ',', dec = '.')

# chi-square test of independence
mt1 <- xtabs(~ETHRACE2A + SEX, data=data)
chisq.test(mt1)

mt2 <- xtabs(~SEX + ADULTCH, data=data)
chisq.test(mt2)   # test is invalid if any of the combinations has less than 5 occurrences

# Fisher test 
mt3 <- xtabs(~ADULTCH + SEX, data=data)
fisher.test(mt3)

# Cochran–Mantel–Haenszel chi-square test 
mytable <- xtabs(~ETHRACE2A+REGION+SEX, data = data)
mantelhaen.test(mytable)  # if p-val < 0.05 then accept NULL hypothesis.

assocstats(mt1) 
 
# visualize using mosaic function. This is better than the mosaicplot() which is default in R
mosaic(ETHRACE2A ~ SEX+REGION, data=data, shade = TRUE)
