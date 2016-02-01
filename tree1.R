# Author - Anupama Rajaram
# Description - program for predicting Survivors on the Titanic 
# Dataset - Kaggle titanic data
# Kaggle score - 0.78947 (~79% correct prediction) 

# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))
options(digits=3)

# libraries needed for this program
library(tree)
library (ISLR)
library(rpart)
library(MASS)

# load training & test datasets
train = read.delim(file = 'train.csv',  header = TRUE, 
                  sep = ',', dec = '.')

test = read.delim(file = 'test.csv', header = TRUE, 
                  sep = ',', dec = '.')

# -------------------- Data management -------------------- #

train$Ticket <- as.character(train$Ticket)
test$Ticket <- as.character(test$Ticket)

train$Cabin <- as.character(train$Cabin)
test$Cabin <- as.character(test$Cabin)

attach(train)  # personal preference - optional

# ------------ using tree() for prediction --------------------- #
tree1 <- tree(Survived ~ Pclass + Sex + Age + SibSp + Parch +Ticket + 
                Fare + Cabin + Embarked, data = train)
summary(tree1)
# Model error rate: 0.177 = 158 / 891 = 17.7%

# model visualization
plot(tree1)
text(tree1 ,pretty =0)
tree1

Pred1 <- predict(tree1, test)
chkdf <- data.frame(PassengerId = test$PassengerId, Survived = Pred1,
                    gender = test$Sex)
chkdf$final[chkdf$Survived <= 0.4]<- 0
chkdf$final[chkdf$Survived > 0.4]<- 1

# create submission file
submit <- data.frame(PassengerId = chkdf$PassengerId, Survived = chkdf$final)
write.csv(submit, file = "jan25_8pm_titanic.csv", row.names = FALSE)

