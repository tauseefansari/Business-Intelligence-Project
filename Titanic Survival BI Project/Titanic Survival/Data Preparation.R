##################################################################################################################
#Part 2 :Data Cleaning and preparation
##################################################################################################################
# Combine train and test data for Data Cleaning and Preparation
Full <- rbind(train,test)

# Structure of the Full data
str(Full)
summary(Full)


# Survival rates in absolute numbers
table(Full$Survived)

# Survival rates in proportions
prop.table(table(Full$Survived))


#Data Type conversion
Full$Pclass = as.factor(Full$Pclass)

#################################################################################
#get percentage of missing value of the attributes - Approach 2 (Function)
sapply(Full, function(df)
{
  sum(is.na(df)==T)/length(df)
})

#Approach - Amelia Package
#install.packages("Amelia")
library("Amelia")
missmap(Full, main = "Missing Map")
###################################################################################
###################################################################################
# Imputing Missing Value

# Missing Value Imputation - Age
Full$Age[is.na(Full$Age)] <- mean(Full$Age,na.rm=T)
sum(is.na(Full$Age))

# Missing Value Imputation - Embarked
table(Full$Embarked, useNA = "always")

# Substitute the missing values with the mode value
Full$Embarked[is.na(Full$Embarked)] <- 'S'
sum(is.na(Full$Embarked))
table(Full$Embarked, useNA = "always")


# Missing Value Imputation - Fare
# Substitute the missing values with the average value
Full$Fare[is.na(Full$Fare)] <- mean(Full$Fare,na.rm=T)
sum(is.na(Full$Fare))


# Missing Value Imputation - Cabin
#Drop the variable as the missing value is more than 20%
Full <- Full[-11]


#Check again for NA
sapply(Full, function(df)
{
  sum(is.na(df)==T)/length(df)
})

# Data Cleaning is done, now we will again split back the data into train and test
# Train test splitting - Why do we need it?
train_cleaned <- Full[1:891,]
test_cleaned <- Full[892:1309,]

