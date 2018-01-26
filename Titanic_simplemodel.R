library(dplyr)
library(nnet)
library(visreg)

#Load data
titanic<-read.csv("C:/Users/julie.wisch/Documents/Kaggle_Titanic/train.csv", header = TRUE)
titanictest <-read.csv("C:/Users/julie.wisch/Documents/Kaggle_Titanic/test.csv", header = TRUE)

#Column Headers:
#Survival (0 = No, 1 = Yes), Pclass (1 = 1st, 2 = 2nd, 3 = 3rd), Sex,
#Age, SibSp (#of siblings/spouses aboard), parch (#ofparents/children aboard)
#ticket (Ticket Number), fare, cabin (cabin number), embarked (Port of embarkation)

#Making some quick tables to look at what might have afected survival
table(titanic$Survived, titanic$Pclass) #better to be first class
table(titanic$Survived, titanic$Sex) #better to be female
table(titanic$Survived, titanic$SibSp) #0 siblings is bad for your chances, so is more than 2
table(titanic$Survived, titanic$Embarked) #doesn't look like it mattered much

#making data into factors for the multinomial model
m.titanic<-titanic


#creating age categories, under 5, 5 - 10, 10 - 15, 15 - 25, 25 - 40, 40 - 60, over 60
m.titanic$Age <- cut(m.titanic$Age, breaks = 5*0:20)
titanictest$Age <- cut(titanictest$Age, breaks = 5*0:20)

#converting variables to factors
m.titanic$Survived <- as.factor(m.titanic$Survived)


m.titanic$PClass<- as.factor(m.titanic$Pclass)
titanictest$Pclass <- as.factor(titanictest$Pclass)
##
m.titanic$SibSp <- as.factor(m.titanic$SibSp)
m.titanic$Parch <- as.factor(m.titanic$Parch) #check for interaction between these two, too
m.titanic$Embarked <- as.factor(m.titanic$Embarked)
titanictest$SibSp <- as.factor(titanictest$SibSp)
titanictest$Parch <- as.factor(titanictest$Parch) 
titanictest$Embarked <- as.factor(titanictest$Embarked)

##Creates a dataframe without any missing data
#titanictest<-na.omit(titanictest)
#m2.titanic<-na.omit(m.titanic)

xtitanictest<-data.frame(1:length(titanictest$Pclass), titanictest$Pclass, titanictest$Sex, 3)
m3.titanic<-data.frame(1:length(m.titanic$Pclass), m.titanic$Pclass, m.titanic$Sex, m.titanic$Survived)

#Fixes column names
names(xtitanictest)[1]<- "Row"
names(xtitanictest)[2]<- "Pclass"
names(xtitanictest)[3]<- "Sex"
names(xtitanictest)[4]<- "Survived"

names(m3.titanic)[1]<- "Row"
names(m3.titanic)[2]<- "Pclass"
names(m3.titanic)[3]<- "Sex"
names(m3.titanic)[4]<- "Survived"

#m3.titanic$Survived <- as.numeric(m3.titanic$Survived)
# for (i in 1:length(m3.titanic$Survived)){
# if (m3.titanic$Survived[i] == 1){m3.titanic$Survived[i] <- 0}
# if (m3.titanic$Survived[i] == 2){m3.titanic$Survived[i] <- 1}}

m3.titanic$Pclass<-as.factor(m3.titanic$Pclass)
simpModel <- glm(Survived ~ Pclass + Sex, data = m3.titanic, family = binomial)


new<- predict(simpModel, xtitanictest)

#Results make sense...more likely to survive if you're first class or female
#Doesn't make sense how probabilities range between -2 and +2 though
plot(xtitanictest$Pclass, new, xlab = "Passenger Class", ylab = "Survival Probability")
plot(xtitanictest$Sex, new, xlab = "Passenger Class", ylab = "Survival Probability")

#normalizing probabilities so they're between 0 and 1
for (i in 1:length(new)){
  new[i] <- ((new[i]+2.5)/5)
}

#Checking on how logical the predictions are...make sense
plot(xtitanictest$Pclass, new, xlab = "Passenger Class", ylab = "Survival Probability")
plot(xtitanictest$Sex, new, xlab = "Passenger Class", ylab = "Survival Probability")

#now making it so that passengers who have 50% or better chance of survival survive, and less than 50% perish
for (i in 1:length(new)){
  if(new[i] >= 0.5){new[i]<-1}
  if(new[i] <  0.5){new[i]<-0}
}


final_simplemodel<-cbind(titanictest$PassengerId, new)


write.csv(final_simplemodel, "C:/Users/julie.wisch/Documents/Kaggle_Titanic/SimpleModelResults.csv")


#Submission Score of 0.765555, place 7246 out of 9678


