# Kaggle-Titanic
This repository contains different solution approaches to the introductory Kaggle project, Titanic:  Machine Learning from Disaster.

The Simple Model uses a GLM and only takes into account passenger sex and passenger class of ticket to predit survival, which it does with 76.6% accuracy.

The Small Neural Net file uses a neural network and only takes into account passenger sex and passenger class of ticket to predict survival.  I was curious if it would perform any better than a simple regression.  It does not.  It also delivers 76.6% accuracy.  

The Big Neural Net file uses a neural network and  takes into account passenger sex, passenger class, passenger age (split into categories of 0-5, 5-10, etc.), number of parents traveling with, and number of siblings traveling with to predict survival.  It delivers 77% accuracy.  I experimented with the number of nodes employed by the neural network, and 4 resulted in the most accurate survival predictions.

