library(dplyr)
library(nnet)
library(visreg)
#library(neuralnet)
library(ggplot2)
set.seed(1000)

#Load data
titanic<-read.csv("C:/Users/julie.wisch/Documents/Kaggle_Titanic/train.csv", header = TRUE)
titanictest <-read.csv("C:/Users/julie.wisch/Documents/Kaggle_Titanic/test.csv", header = TRUE)

#Column Headers:
#Survival (0 = No, 1 = Yes), Pclass (1 = 1st, 2 = 2nd, 3 = 3rd), Sex,
#Age, SibSp (#of siblings/spouses aboard), parch (#ofparents/children aboard)
#ticket (Ticket Number), fare, cabin (cabin number), embarked (Port of embarkation)

#making data into factors for the multinomial model
m.titanic<-titanic

#creating age categories, under 5, 5 - 10, 10 - 15, 15 - 25, 25 - 40, 40 - 60, over 60
m.titanic$Age <- cut(m.titanic$Age, breaks = 5*0:20)
titanictest$Age <- cut(titanictest$Age, breaks = 5*0:20)

#converting variables to factors
m.titanic$Survived <- as.factor(m.titanic$Survived)
m.titanic$PClass<- as.factor(m.titanic$Pclass)
titanictest$Pclass <- as.factor(titanictest$Pclass)
m.titanic$SibSp <- as.factor(m.titanic$SibSp)
m.titanic$Parch <- as.factor(m.titanic$Parch) #check for interaction between these two, too
m.titanic$Embarked <- as.factor(m.titanic$Embarked)
titanictest$SibSp <- as.factor(titanictest$SibSp)
titanictest$Parch <- as.factor(titanictest$Parch) 
titanictest$Embarked <- as.factor(titanictest$Embarked)


#creating a column on the end of xtitanictest for survival numbers to go
xtitanictest<-data.frame(titanictest$PassengerId, titanictest$Pclass, titanictest$Sex, 
                         titanictest$Age, titanictest$SibSp, titanictest$Parch, titanictest$Embarked,
                         3)
m3.titanic<-data.frame(m.titanic$PassengerId, m.titanic$PClass, m.titanic$Sex, 
                       m.titanic$Age, m.titanic$SibSp, m.titanic$Parch, m.titanic$Embarked,
                       m.titanic$Survived)

#Fixes column names
names(xtitanictest)[1]<- "PassengerId"
names(xtitanictest)[2]<- "Pclass"
names(xtitanictest)[3]<- "Sex"
names(xtitanictest)[4]<- "Age"
names(xtitanictest)[5] <- "SibSp"
names(xtitanictest)[6] <- "Parch"
names(xtitanictest)[7] <- "Embarked"
names(xtitanictest)[8] <- "Survived"

names(m3.titanic)[1]<- "PassengerId"
names(m3.titanic)[2]<- "Pclass"
names(m3.titanic)[3]<- "Sex"
names(m3.titanic)[4]<- "Age"
names(m3.titanic)[5] <- "SibSp"
names(m3.titanic)[6] <- "Parch"
names(m3.titanic)[7] <- "Embarked"
names(m3.titanic)[8] <- "Survived"

# Encode as a one hot vector multilabel data for neural network processing
train.Pclass <- class.ind(as.factor(m3.titanic$Pclass))
colnames(train.Pclass)<- c("Pcl1", "Pcl2", "Pcl3")
train.Sex <- class.ind(as.factor(m3.titanic$Sex))
colnames(train.Sex)<- c("Xl1", "Xl2")
train.Age <- class.ind(as.factor(m3.titanic$Age))
colnames(train.Age)<- c("Al1", "Al2", "Al3", "Al4", "Al5", "Al6", "Al7", "Al8", "Al9", "Al10",
                           "Al11", "Al12", "Al13", "Al14", "Al15", "Al16", "Al17", "Al18", "Al19", "Al20")
train.Survived <- class.ind(as.factor(m3.titanic$Survived))
colnames(train.Survived)<- c("Sl1", "Sl2")


test.Pclass <- class.ind(as.factor(xtitanictest$Pclass))
colnames(test.Pclass)<- c("Pcl1", "Pcl2", "Pcl3")
test.Sex <- class.ind(as.factor(xtitanictest$Sex))
colnames(test.Sex)<- c("Xl1", "Xl2")
test.Age <- class.ind(as.factor(xtitanictest$Age))
colnames(test.Age)<- c("Al1", "Al2", "Al3", "Al4", "Al5", "Al6", "Al7", "Al8", "Al9", "Al10",
                        "Al11", "Al12", "Al13", "Al14", "Al15", "Al16", "Al17", "Al18", "Al19", "Al20")
test.Survived <- class.ind(as.factor(xtitanictest$Survived))
colnames(test.Survived)<- c("Sl1")

#These guys are numbers that have meaning rather than factor levels, so leave them alone, just add them to the training set
train.SibSp <- m3.titanic$SibSp
train.Parch<-m3.titanic$Parch
test.SibSp <- xtitanictest$SibSp
test.Parch<-xtitanictest$Parch

##Normalizing data##
train.SibSp <- scale(as.numeric(train.SibSp), center = FALSE, scale = TRUE)
train.Parch <- scale(as.numeric(train.Parch), center = FALSE, scale = TRUE)
test.SibSp <- scale(as.numeric(test.SibSp), center = FALSE, scale = TRUE)
test.Parch <- scale(as.numeric(test.Parch), center = FALSE, scale = TRUE)

maybe<-cbind(train.SibSp, train.Parch)
colnames(maybe)<-c("SibSp", "Parch")

testmaybe<-cbind(test.SibSp, test.Parch)
colnames(testmaybe)<-c("SibSp", "Parch")


trainingset <- cbind(train.Pclass, train.Sex, train.Age, maybe, train.Survived)
testset <- cbind(test.Pclass, test.Sex, test.Age, testmaybe, test.Survived)


## 1. Fit a Single Hidden Layer Neural Network using Least Squares
train.nnet<-nnet(Sl1~ Pcl1 + Pcl2 + Pcl3 + Xl1 +Xl2 +
                   Al1 + Al2 + Al3 + Al4 + Al5 + Al6 + Al7 + Al8 + Al9 + Al10 +
                   Al11 + Al12 + Al13 + Al14 + Al15 + Al16 + Al17 + Al18 + Al19 + Al20 +
                   SibSp + Parch,trainingset, size = 4, Hess=FALSE,decay=15e-4,maxit=1000)


plot.nnet(train.nnet)

## Use TEST data for testing the trained model
test.nnet<-predict(train.nnet,testset)

new<-test.nnet

###The closer the test.nnet marker is to zero, the more likely the person is to survive
###The closer test.nnet is to one, the more likely the person is to perish
###Mistakenly inverted those so its a bit counterintuitive, but easily fixed below....

#now making it so that passengers who have 50% or better chance of survival survive, and less than 50% perish
for (i in 1:length(new)){
  if(new[i] >= 0.5){new[i]<-2}
  if(new[i] <  0.5){new[i]<-1}
  if(new[i] == 2){new[i]<-0}
}



#setting up and writing the results to a file
final_neuralnet<-cbind(xtitanictest$PassengerId, new)

write.csv(final_neuralnet, "C:/Users/julie.wisch/Documents/Kaggle_Titanic/BigNeuralModelResults2.csv")

#This scores a 73.2%
#I would have expected it to perform better....




