##################################################################################################################
#Part 6 : # Build Decision Tree Model
##################################################################################################################
#install.packages('rattle')
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')

library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)

# Recreate the gender model
dtree1 <- rpart(Survived ~ Sex, data=train_Featured, method="class")
rpart.plot(dtree1)
fancyRpartPlot(dtree1)
my_Prediction <- predict(dtree1, test_Featured, type = "class")

my_solution <- data.frame(PassengerId = test_Featured$PassengerId, Survived = my_Prediction)
write.csv(my_solution, file = "D://Tauseef//Titanic//dtree1.csv", row.names = FALSE)
######################################################################################
#Build a deeper tree without feature enginnering
dtree2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
              data=train_Featured,
              method="class")

rpart.plot(dtree2)
fancyRpartPlot(dtree2)


my_Prediction <- predict(dtree2, test_Featured, type = "class")

my_solution <- data.frame(PassengerId = test_Featured$PassengerId, Survived = my_Prediction)
write.csv(my_solution, file = "D://Tauseef//Titanic//dtree2.csv", row.names = FALSE)

######################################################################################
# Build a deeper tree - with feature engineering
dtree3 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Child +Title + FamilySize,
             data=train_Featured,
             method="class")

rpart.plot(dtree3)
fancyRpartPlot(dtree3)


my_Prediction <- predict(dtree3, test_Featured, type = "class")

my_solution <- data.frame(PassengerId = test_Featured$PassengerId, Survived = my_Prediction)
write.csv(my_solution, file = "D://Tauseef//Titanic//dtree3.csv", row.names = FALSE)
######################################################################################
# Go all over and reach max depth of the tree by making cp =0 and minsplit = 2
dtree4 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Child +Title + FamilySize,
             data=train_Featured,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))

fancyRpartPlot(dtree4)
my_Prediction <- predict(dtree4, test_Featured, type = "class")
my_solution <- data.frame(PassengerId = test_Featured$PassengerId, Survived = my_Prediction)
write.csv(my_solution, file = "D://Tauseef//Titanic//dtree4.csv", row.names = FALSE)
#Accuracy = 69% -> worst than gender model -> Welcome to overfitting
######################################################################################
# Let's handle Overfitting Problem
printcp(dtree4) 
#Minimum error occurs when the tree size is = 3

#Find the value of CP for which cross validation error is minimum
min(dtree4$cptable[,"xerror"])
which.min(dtree4$cptable[,"xerror"])
cpmin <- dtree4$cptable[3, "CP"]
cpmin


#Prune the tree by setting the CP parameter as =  cpmin
dtree5 = prune(dtree4, cp = cpmin)
rpart.plot(dtree5)
fancyRpartPlot(dtree5)

my_Prediction <- predict(dtree5, test_Featured, type = "class")
my_solution <- data.frame(PassengerId = test_Featured$PassengerId, Survived = my_Prediction)
write.csv(my_solution, file = "D://Tauseef//Titanic//dtree5.csv", row.names = FALSE)

#Alternative way of pruning tree
dtree5 <- prp(dtree4,snip=TRUE)$obj
fancyRpartPlot(dtree5)

my_Prediction <- predict(dtree5, test_Featured, type = "class")
my_solution <- data.frame(PassengerId = test_Featured$PassengerId, Survived = my_Prediction)
write.csv(my_solution, file = "D://Tauseef//Titanic//dtree6.csv", row.names = FALSE)

######################################################################################
