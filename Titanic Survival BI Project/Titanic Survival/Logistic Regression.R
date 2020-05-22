##################################################################################################################
#Part 5 : Logistics Regression 
##################################################################################################################
################################################################################
# Step 1:Split data in train and test data
#install.packages("caTools")
library(caTools)
set.seed(390)
split <- sample.split(train_Featured, SplitRatio = 0.8)
split

train.data <- subset(train_Featured, split== "TRUE")
test.data <- subset(train_Featured, split== "FALSE")

str(train.data)
str(test.data)

####################################################################################
# Step 2:Train model with logistics regression using glm function

# Model fitting - Try 1:without Feature Enginnering
#After removing Passenger ID, Name and Ticket, Child, Title. Family Size
logit_model1 <- glm(Survived ~ .,family=binomial(link='logit'),data = train.data[-c(1,4,9,12,13,14)])
summary(logit_model1) #AIC : 626.99


# Model fitting - Try 2 : With Feature Engineering
#After removing Passenger ID, Name and Ticket
logit_model2 <- glm(Survived ~ .,family=binomial(link='logit'),data = train.data[-c(1,4,9,13)])
summary(logit_model2) # AIC : 597 :  PClass, Age, SibSp, Parch
# AIC (lesser the better, used for comparing different models)

anova(logit_model1, logit_model2, test = "Chisq")


anova(logit_model2, test = 'Chisq')

#######################################################################################
# Step 3:Predict test data based on trained model -logit_model
fitted.results <- predict(logit_model2,newdata=test.data,type='response')

#######################################################################################
# Step 4: Change probabilities to class (0 or 1/Yes or No)
# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- ifelse(fitted.results > 0.5,1,0)

#######################################################################################
# Step 5: Evauate Model Accuracy using Confusion matrix
library(caret) 
library(lattice)
library(e1071)
confusionMatrix(table(test.data$Survived, fitted.results))

# ROC-AUC Curve
#install.packages("ROCR")
library(ROCR)
ROCRPred <- prediction(fitted.results, test.data$Survived)
ROCRPerf <- performance(ROCRPred, measure ="tpr", x.measure ="fpr")

par(mfrow = c(1, 1))
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),main = "ROC CURVE")
abline(a=0, b=1)

auc <- performance(ROCRPred, measure = "auc")
auc <- auc@y.values[[1]]
auc <- round(auc, 4)
legend (.6,.4,auc, title = "AUC", cex =1)

####################################################################################
# Make predictions on the test set
my_prediction <- predict(logit_model2, test_Featured, type = "response")

# If prob > 0.5 then 1, else 0. Threshold can be set for better results
my_prediction <- ifelse(my_prediction > 0.5,1,0)


# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test_Featured$PassengerId, Survived = my_prediction)


# Use nrow() on my_solution - Should be 418
nrow(my_solution)

str(my_solution)

# Finish the write.csv() call
write.csv(my_solution, file = "D://Tauseef//Titanic//my_solution_logit.csv", row.names = FALSE)
##########################################################################################
logit_model3 <- glm(Survived ~ .,family=binomial(link='logit'),data = train_Featured[-c(1,4,9)])
summary(logit_model3) 

my_prediction <- predict(logit_model3, test_Featured, type = "response")

my_prediction <- ifelse(my_prediction > 0.5,1,0)

my_solution <- data.frame(PassengerId = test_Featured$PassengerId, Survived = my_prediction)

nrow(my_solution)

write.csv(my_solution, file = "D://Tauseef//Titanic//my_solution_logit2.csv", row.names = FALSE)

