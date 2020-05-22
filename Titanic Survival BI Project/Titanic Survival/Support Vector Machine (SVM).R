# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)

svm_model<-svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Child +Title + FamilySize, data=train_Featured, method = "class")
summary(svm_model)

my_Prediction <- predict(svm_model, test_Featured, type = "class")

my_solution <- data.frame(PassengerId = test_Featured$PassengerId, Survived = my_Prediction)
write.csv(my_solution, file = "D://Tauseef//Titanic//svm.csv", row.names = FALSE)

