# Author - Anupama Rajaram
# Description - randomforest algorithm in R for predicting Survivors on the Titanic 
# Dataset - Kaggle titanic data
# Kaggle score - 0.76555 (~76% correct prediction)  for cutoff = 0.5
#         score = 0.77512 for cutoff = 0.4 

# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))
options(digits=3)

# load libraries 
library(tree)
library (ISLR)
library(rpart)
library(MASS)
library (randomForest)

# load datasets
train = read.delim(file = 'train.csv',  header = TRUE, 
                  sep = ',', dec = '.')
                  
test = read.delim(file = 'test.csv', header = TRUE, 
                  sep = ',', dec = '.')
                  
# --------------- Data preparation and management -------------------- #
train$Ticket <- as.character(train$Ticket)
test$Ticket <- as.character(test$Ticket)

train$Cabin <- as.character(train$Cabin)
test$Cabin <- as.character(test$Cabin)

train$Age[is.na(train$Age)] <- 28.0   # 177 NAs in age column, so marking them with
                                      # median value = 28.0
test$Age[is.na(test$Age)] <- 27.0   # 86 NAs in age column, so marking them with
                                    # median value = 27.0
                                    
attach(train)

# ----------------- Prediction starts here ---------------------------- #
set.seed(17) # for reproducability
b1 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp, data = train,
                        importance =TRUE)
# error rate from model = 15.4%

# visualize model 
summary(b1)
plot(b1)

# predict values on the test data
Predrf <- predict(b1, test)
chkrf <- data.frame(PassengerId = test$PassengerId, Survived = Predrf,
                    gender = test$Sex)

# convert probabilities into 0/1 predictions, as needed for Submission
chkrf$final[chkrf$Survived <= 0.5]<- 0
chkrf$final[chkrf$Survived > 0.5]<- 1  

# cutoff = 0.4 - to change the cutoff, simply increase/ decrease the value in 
# two statements below
# chkrf$final[chkrf$Survived <= 0.4]<- 0
# chkrf$final[chkrf$Survived > 0.4]<- 1  

# create .csv file for submission 
submit <- data.frame(PassengerId = chkrf$PassengerId, Survived = chkrf$final)
write.csv(submit, file = "rf_titanic.csv", row.names = FALSE)


