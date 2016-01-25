##############################################################################
Author -  Anupama Rajaram
Program - Clustering Chart function for production and sales volume. 
          Code modified from snippets provided in Business Analytics Course
          on Coursera platform, from ESSEC Business School.
Output -  A graph of CV versus Groups should result from this code.
          The graph has datapoints clearly divided into 3 clusters, each with 
          a different title and plotted with colors red, blue and green.
##############################################################################
# Set your directory to the folder where you have downloaded the SKU dataset
# file DATA_2.01_SKU.csv is located.

# Clean up memory from all previous R sessions
rm(list=ls(all=TRUE))

# Loading data from our .csv file into an object named "data"
data=read.table('DATA_2.01_SKU.csv', header = T,sep=',') 
# The function read.table enables us to read flat files such as .csv files

# Let's start with some summary statistics to get a feel for the data
str(data)   # The str() function shows the structure of the dataset 
            # and details the type of variables that it contains

summary(data)   # summary() function provides minimum, mean, maximum and 
                # quartilesfor each variable in the dataset

# Plot data to see if we can identify groups visually 
plot(data$CV, data$ADS, main = "SKU Example", 
     ylab="Average Daily Sales", xlab= "Coefficient of Variation")
abline(v=0.2, col = "red")  # we can draw a vert. line by using abline fn
                            # and passing it the v argument
abline(h=4, col="red")      # we can draw horz line by using abline fn
                            # and passing it the h argument

text(0.15,9.7, "Horses", col = "red") 
# add text to our plot using text fn(), to label the group "Horses"
#0.15 = x co-ordinate, 9.7 = y co-ordinate
# adding values outside the graph does not throw an error, but naturally 
# the text will not be printed either.

text(0.65,9, "Wild Bulls", col = "red") # and group "Wild Bulls"
text(0.8,2, "Crickets", col = "red") # and group "Crickets"

#=========this is an added line for testing purposes============#
text(0.3,14, "Anu", col = "purple")
#=========Feel free to delete these 3 lines of code============#

# Let's find groups using hierarchical clustering and check if we 
# obtain similar results
testdata=data  # To keep dataset safe, create a copy called "testdata"
testdata = scale(testdata) # To keep dataset safe, create copy 
                            # called "testdata". Note scale fn whose 
                            # default method centers and/or scales columns
                            # of a numeric matrix.
                            
# scale fn() automatically performs data normalization on all variables


d = dist(testdata, method = "euclidean") 
# Above dist fn() computes distances of all the observations in our dataset

hcward = hclust(d, method="ward.D")
# hclust() function performs hiearchical clustering, we pass it
# the distances, and we set the method argument to "ward.D"

data$groups<-cutree(hcward,k=3) # assign our points to our k=3 clusters 

# The lattice library provides a complete set of functions for producing advanced plots.
install.packages("lattice") #install the lattice package by using the install.packages() function
library(lattice) # load the lattice package by using the library() function and passing it the name of the package you wish to load

xyplot(ADS~ CV,main = "After Clustering", type="p",group=groups,data=data, 
       # define the groups to be differentiated 
auto.key=list(title="Group", space = "left", cex=1.0, just = 0.95), 
# to produce the legend we use the auto.key= list() 
       par.settings = list(superpose.line=list(pch = 0:18, cex=1)), 
# the par.settings argument allows us to pass a list of display settings
       col=c('blue','green','red')) 
# finally we choose the colour of our plotted points per group
