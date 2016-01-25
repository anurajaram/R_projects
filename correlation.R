# ---------------------------------------------------------------------------- #
#   Author - Anupama Rajaram
#   Program - correlation tests
#             visualize using corrgram()
# ---------------------------------------------------------------------------- #

 # prepping the environment - clean up memory, set number formatting
 rm(list=ls(all=TRUE))
 options(digits=2)
 
 library("MASS")  # to load dataset Boston
 library(corrgram) # for the 
 
 b <- data.frame(Boston)
 attach(b)
 
 # tests for correlation
 cor(age,crim, method = "kendall" ) # kendall correlation 
 
 cor(age, crim ) # default = pearson correlation
 cor(age, crim, method = "pearson" ) # kendall correlation 

 cor(age,crim, method = "spearman" ) # spearman correlation 
 
 cor(b)
 corrgram(b, order=TRUE, lower.panel=panel.shade,
          upper.panel=panel.pie, text.panel=panel.txt,
          main="Correlogram of intercorrelations in Boston Crime dataset")
 
 # corrgram with lower triangle = smoothed fit lines and confidence ellipses
 # and scatter plots in the upper triangle
 corrgram(b, order=TRUE, lower.panel=panel.ellipse,
          upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.minmax,
          main="Correlogram for Boston Crime dataset") 
 
 # changing colors in the graph using col.regions=colorRampPalette() option
  corrgram(b, order=TRUE, lower.panel=panel.shade,
          upper.panel=panel.pie, text.panel=panel.txt,
          col.regions=colorRampPalette(c("red", "blue","orange")),
          main="A Corrgram with New Colors")
 
 corrgram(b, lower.panel=panel.shade,
          upper.panel=NULL, text.panel=panel.txt,
          main="Corrgram")
 
 
