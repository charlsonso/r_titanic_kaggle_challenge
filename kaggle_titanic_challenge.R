# Tutorial Based on https://www.youtube.com/watch?v=Zx2TguRHrJE
# Binary Classification
setwd("~/Desktop/repos/r")

#Add data into dataframe
#header=TRUE, sets dataframe header. By default, always on.
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header=TRUE)
# run in interpreter, check for errors in read in

titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header=TRUE)
#ctrl-enter will run line in the console

# Get global median
# median(titanic.train$Age, na.rm=TRUE)
# median(titanic.test$Age, na.rm=TRUE)

# Create column IsTrainSet and set all values to true
titanic.train$IsTrainSet <- TRUE
#tail(titanic.train$IsTrainSet)
titanic.test$IsTrainSet <- FALSE

#Fill the test set with unkown to match the same columns leangth in train and test
titanic.test$Survived <- NA

# R equivalent of Union for dataset
titanic.full <- rbind(titanic.train, titanic.test)

#Gives a overview of the column data (tells us how many false and true)
#table(titanic.full$IsTrainSet)

titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

#global median is the same as global
age.median <- median(titanic.full$Age, na.rm = TRUE)

# Lots of empty data in Age Column, replace it with the median
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

#is.na(titanic.full$Fare)

#clean missing values of fare
fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

# categorical casting (pass it the order)
titanic.full$Pclass <- as.factor(titanic.full$Pclass) 
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

# split dataset back out into train and test
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)

#Builing a predictive model
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry =3, nodesize = 0.01 * nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId

output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file="r_titanic_kaggle_challenge/solution.csv", row.names = FALSE)
