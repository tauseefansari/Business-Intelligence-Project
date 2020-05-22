##################################################################################################################
#Part 1: Creating First Prediction Model
##################################################################################################################
# Titanic Dataset
# Import the training and test dataset
train <- read.csv (file.choose(), stringsAsFactors = F,na.strings=c("","NA"," "))
test <- read.csv (file.choose(), stringsAsFactors = F,na.strings=c("","NA"," "))

str(train)
str(test)
summary(train)
summary(test)

########################################################################
#Lazy Predictor (Creating First Predictor)
########################################################################
# Initialize a Survived column to 0
test$Survived <- 0
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

# Check the row number - It should match with test row number - 418
nrow(my_solution)

#write the solution for submission
write.csv(my_solution, file='D://Tauseef//Titanic//LazyPredictor.csv', row.names = FALSE)

########################################################################
#First Predictor with Gender
##################################################################################################################
# Set Survived to 1 if Sex equals "female"
test$Survived [test$Sex == "female"] <- 1

my_solution <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

# Check the row number - It should match with test row number - 418
nrow(my_solution)

#write the solution for submission
write.csv(my_solution, file = "D://Tauseef//Titanic//Gender_Model.csv", row.names = FALSE)

