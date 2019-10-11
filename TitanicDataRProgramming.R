# YouTube series about Titanic Kaggle Competition by David Langer
train <- read.csv("train.csv", header=TRUE)
test <- read.csv("titanic.csv", header=TRUE)


# add 'Survived' column to TEST and give "NONE" value for all data
test.survived <- data.frame(Survived = rep("NONE", nrow(test)), test[,])


#combine TRAIN and TEST (with 'survived' column) data sets into one
data.combined <- rbind(train, test.survived)


# to check the class STRucture of the data.frame 'data.combined'
str(data.combined)


# change 'survived' & 'pclass' class types to factors
## machine learning / data analysis prefers factors over integer
## and character class types
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


# show the gross survival rates
table(data.combined$Survived)


# distribution of social classes (pclass: 1=upper class, 2=middle
# class, 3= low class)
table(data.combined$Pclass)


# load up ggplot 2 function for uses of visuals
library(ggplot2)


# hypothesis - rich people survived at higher rates
## turn trains Pclass into factor class type
train$Pclass <- as.factor(train$Pclass)

# plot histogram comparing survival rate of different classes
# geom_bar(stat = "count") instead of histogram b/c newer version(?)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat = "count") +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


# examine first few names listed
head(as.character(train$Name))


# how many unique names are there across both train and test?
# for predictive models, want to find anomalies such as duplicate names
length(unique(as.character(data.combined$Name)))


# two duplicate names, first get the duplicates and store
# them as a vector
dup.names <- as.character(data.combined[which
              (duplicated(as.character(data.combined$Name))), "Name"])


# next, look at records from combined data set
data.combined[which(data.combined$Name %in% dup.names),]


# what's up with the 'Miss.', 'Mr.' thing?
library(stringr)


# is there correlation between "Miss.', etc. titles and other
# variables (ex. sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
# "1:5" is range of row 1 through row 5, and "," just shows all columns
# available instead of a range of columns
## finding first five passengers with 'Miss.' title
misses[1:5,]


# hypothesis - name titles (Mrs. vs Miss., etc.) correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]


# check out males to see if pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]


# expand on relationship between 'Survived' and 'Pclass'by adding 'Title'
# variable to data set and explore potential 3-dimensional relationship

# create utility function for title extraction
extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if(length(grep("Miss.", Name)) > 0) {
    return("Miss.")
  } else if(length(grep("Master.", Name)) > 0) {
    return("Master.")
  } else if(length(grep("Mrs.", Name)) > 0) {
    return("Mrs.")
  } else if(length(grep("Mr.", Name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

titles <- NULL
for(i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
# turn new 'Title' variable into factors because they're better for
# data analysis than strings
data.combined$Title <- as.factor(titles)


# since we only have survived data for 'train' data set, only use first
# 891 rows
## fact_wrap splits the desired variable (Pclass) into different graphs
## for each of that variable's elements
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar(stat = "count") +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

## we now notice that women/children are MUCH more likely to survive than men


# what is the distribution between males and females across train & test?
table(data.combined$Sex)

# visualize 3-D relationship between sex, pclass, and survival
## only know survival rates of train so plot only first 891 passengers
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar(stat = "count") +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Cunt") +
  labs(fill = "Survived")


# age and sex seem important from analysis of 'title' now lets look at
# distributions of age of whole data set
summary(data.combined$Age)


# to be thorough, look at survival rate broken out by sex, pclass, & age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")
  

# we want to validate that "Master." title is proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

## max age is 14.5--> "Master." should = male child therefore we can simply
## refer to adult men as "Mr." and male children as "Master."


# "Miss." is more complicated, let's examine
misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)


# "Miss." plot to visualize distribution
ggplot(misses[misses$Survived != "NONE",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass")
  xlab("Age") +
  ylab("Total Count")
  

## seems that female children may have different survival rate
## could be candidate for feature engineering later
# trying to find women travelling alone (in 'misses')
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))   #14.5 because that's max age for 'Master'


# moving on to SibSp (siblings + spouse) variabel--> summarize
summary(data.combined$SibSp)
## median=0 means at least half people don't have siblings/spouse aboard
## SibSp skews highly towards zero


# can we treat SibSp as a factor?
length(unique(data.combined$SibSp))
## there are 7 uniqe values for SibSp ie) value of 1 SibSp, 2 SibSp, etc.

# now to factor SibSp
data.combined$SibSp <- as.factor(data.combined$SibSp)


# we believe title is predictive; visualize survival rates by SibSp, Pclass, & title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_histogram(stat = "count") +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
## all titles, but especially children in small families seem to have higher
## survival rates (something to keep an eye on)


# treat parch (parents/children) variable as factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_histogram(stat = "count") +
  facet_wrap(~ Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
## possible that fewer family members equals better survival rate?


# try some FEATURE ENGINEERING.. what about creating family size feature?
## combine train, test sets of 'Sibsp' & 'Parch' variables and add together for
## 'family size' variable
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch +1)
# "+1" is for including each individual as family


# visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  geom_histogram(stat = "count") +
  facet_wrap(~ Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("familuy.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# take a look at ticket variable
str(data.combined$Ticket)


# because 'ticket' has so many levels, it's more likely a string, not a factor
# convert to string and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]


# there's not immediately apparent structure to the data, let's try to find some
# we'll start by looking at just the first character of each ticket
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)


# now we can make a factor for analysis and visualization
data.combined$ticket.first.char <- as.factor(ticket.first.char)


# now to plot
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")


# ticket might be predictive, let's dive further and compare to Pclass
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")


# last, let's see if we get a pattern between pclass and title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")
## there's not much "signal" telling us that ticket is predictive so we'll
## discard it for now


# now we're gonna work with the fares paid by passengers
summary(data.combined$Fare)
length(unique(data.combined$Fare))


# can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)


# let's see if fare has predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")
## fare doesn't seem to have much predictive power, ie. doesn't wow us with
## new info


# analysis of the cabin variable - look at structure of 'cabin'
str(data.combined$Cabin)


# cabin isn't really a factor, make into string and display first 100
## factors with more than 32 levels is usually a string (character)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]


# replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""),"Cabin"] <- "U"
data.combined$Cabin[1:100]
## possible that higher character means higher placement on ship, which means
## closer proximity to lifeboats, which means higher survival rate

# take a look at first char as factor
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)


# add to data.combined as new variable
data.combined$cabin.first.char <- cabin.first.char


# plot cabin.first.char
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")


# could have predictive power, compare to pclass
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Pclass") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")
## cabin might not be important because it corresponds to pclass, which we
## already know is important and probably predictive


# is it more helpful to include title??
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")


# what about folks with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")
## not too much going on here either


# does survivability depend on where you got on board the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)


# plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
## again, not particularly important or indicative

## we now know that Pclass and Title (b/c it combines age and sex) are the best indicators
## for predictive models and that family size might be helpful as well

#### MODELING!!!!!####


# random forest is a simple predictive algorithm
library(randomForest)

# train a Random Forest with default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1

## OOB (Out Of Box) estimate of error rate: 20.99% -> ~ 79% of the time this random forest
## is successful in predicting survival rates using only Pclass and Title variables
## which is pretty good!

## Confusion Matrix -> 0:0 = correctly predicting a death, 0:1 =  incorrectly predicting
## a death, 1:0 = incorrectly predicting survival, 1:1 = correctly predicting survival
## This model was correct ~98% of the time predicting a death, but only ~49% correct
## when predicting survival

varImpPlot(rf.1)

## the further the variable places on the graph towards the right, the more important
## that variable is for predictive power.. turns out Title is more predictive than Pclass


# train a random forest using Pclass, Title, & Sibsp
rf.train.2 <- data.combined[1:891, c("Pclass","Title","SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2

## OOB error rate: 19.53% -> better than 1% improvement from last OOB error rate
## Class error for death prediction = 11.29%, class error for survival = 32.75%
## predicting survival improved by a lot, but predicting death became less accurate

varImpPlot(rf.2)

## Title is still most predictive, SibSp is even less predictive than Pclass


# now let's make a model using Pclass, Title, & Parch instead of SibSp
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3

## error rate = 19.87%; a little less accurate than SibSp model, but better than original
## predicting death = 9.65% error ~ 1-2% better than SibSp model
## predicting survival = 36.26% error ~ 4% worse than SibSp model
## overall it's a little less predictive/useful than SibSp

varImpPlot(rf.3)


# let's use all of these variables in one model
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4

## error rate = 19.08%, our most accurate model yet!!
## 10.74% error rate in death prediction, 32.45% error rate in survival prediction
## very similar to SibSp model

varImpPlot(rf.4)

# train a model using Pclass, Title, and family.size to combine SibSp and Parch
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5

## error rate = 18.18%, almost 3% better than original, by far best model so far

varImpPlot(rf.5)

# train a Random Forest using Pclass, Title, SibSp, & family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6

## error rate = 19.64%, signifcantly worse than only family.size model

varImpPlot(rf.6)

# train a Random Forest using Pclass, Title, Parch, & family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7

## ALERT: my model using Parch + family.size is more accurate than SibSp + family.size
## but the oppostie is true for the video demonstration (not sure why)

varImpPlot(rf.7)



# Before getting into features engineering we need to establish a methodology for 
# estimating our error rate on the test set (ie. unseen data). This is critical, because
# without this we are more likely to overfit. Let's start with a submission of rf.5 to
# Kaggle to see if our OOB error estimate is accurate.


# subset our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "Title", "family.size")]

# make predictions
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

## 'predict' function is important: used to (spoiler!) predict your models

# write out CSV file for submission to Kaggle
## 'rep' function replicates elements of vector, so recreating the elements of test 
## data set then predicting which ones survive and perish using 'rf.5.preds'
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20190906_1.csv", row.names = FALSE)

# submission score is .79425, significantly worse than .8182 OOB success rate

# let's look into cross-validation using the 'caret' package to see if we can
# get more accurate predictions
library(caret)
library(doSNOW)

# Research shows that 10 fold Cross Validation repeated 10 times is the best place to 
# start, there are no hard rules on it. This is where the experience of the data scientist
# (ie. the "art") comes into play. We'll start with 10 fold CV repeated 10 times and 
# go from there.

# leverage caret to create 100 total folds, but ensure that the ratio of those that
# survived and perished in each fold matches the overall training set. This is known
# as STRATIFIED cross validation an generally produces better results.
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)
## cv.10.folds is basically the index for using later in the trainControl


# check stratification
table(rf.label)

table(rf.label[cv.10.folds[[33]]])
## checking the results of each ratio, they both have essentially the same perish rate
## of ~62% which means that it is stratified

# set up caret's trainControl object for the cross validation
## trainControl is VERY important!! Holds the predictive algorithms like boot strapping,
## cross validation, etc.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)


# set up doSNOW package for multi-core training. This is helpful b/c we're going
# to be training a lot of trees.
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


#set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

# shutdown cluster
stopCluster(cl)

# check out results
rf.5.cv.1

## estimates error rate of 18.75% which is only slightly more pessimistic
## than rf.5 alone (OOB = 18.18% error rate), so not pessimistic enough



# now lets retry that with fewer folds because fewer folds makes models
# less prone to overfit. less data to train with means less prone to
# overfitting
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)
## number = 5 instead of number = 10


cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

stopCluster(cl)

rf.5.cv.2

## based on this test, 5-fold CV is not better because it's accuracy is even less
## pessimistic

# let's try 3-fold CV repeated 10 times
## 3-fold might be best bet because the training data for Titanic is roughly 
## 2/3 of the whole dataset. In very basic terms 3-fold folds the data into
## 1/3's and compares to each increment, so 3-fold might fit best with our
## data.
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)
## number = 3 instead of number = 10


cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)
## not sure why ntree = 64 and not 1000...##

stopCluster(cl)

rf.5.cv.3

## The 3-fold CV repeated 10 times was the most pessimistic model, most closely
## resembling the accuracy rate we submitted to Kaggle. Moving forward, we're
## going to continue working with 3-fold, instead of 5 / 10-fold.



# Let's create a single decision tree to better understand what's going on with
# our features. Although random forests are much more powerful, single decision
# trees have the advantage of being easier to understand.

# install and load packages
library(rpart)
library(rpart.plot)

# use 3-fold CV repeated 10 times
# create utility function
rpart.cv <- function(seed, training, labels, ctrl){
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30,
                    trControl = ctrl)
  
  # shutdown cluster
  stopCluster(cl)
  return(rpart.cv)
}

# grab features
features <- c("Title", "Pclass", "family.size")
rpart.train.1 <- data.combined[1:891, features]

# run CV and check results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1
## cp = .03085299; accuracy = 81.705% --> higher than random forest with same
## associated features.. that's expected b/c random forests try to counteract
## individual decision trees tendency to overfit. Since we did only 1 individual
## tree, the overfitting occurred and the accuracy came out as higher than expected


# plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

## the plot takes three tests --> 1) is your title "Mr." or "Other"? If yes,
## the model predicts that person perishes. If not we go onto the second test
## which is... 2) are you in Pclass 3? If no, the model predicts you survives. If
## that person IS in Pclass 3, then we go onto the final test... 3) is the family
## size equal to 5,6,8, or 11? If yes, the model predicts that person to perish
## and if no it predicts him/her to survive (although the accuracy of that
## prediction is not very high with a proportion of 83/138).


# A couple of things jump off as interesting/intriguing:
#      1) Titles of "Mr." or "Other" are predicted to perish at an accuracy rate
#         of 83.2%.
#      2) Titles of "Mrs.", "Miss.", or "Master." in 1st or 2nd class is 
#         predicted to survive with 94.9% accuracy.
#      3) Titles of "Mrs.", "Miss." or "Master." in 3rd class with family sizes
#         of 5,6,8,11 are predicted to perish with 100% accuracy.
#      4) Titles of "Mrs.", "Miss." or "Master." in 3rd class with famimly sizes
#         not equal to 5,6,8,11 are predicted to survive with only 59.6% accuracy.


# both rpart and random forest agree that Title is important.. let's investigate
table(data.combined$Title)

# parse out last name and Title
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# add last names to data frame for later on just in case
data.combined$Last.Name <- last.names

# now for Titles
name.Split <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.Split, "[", 2)
unique(titles)


# what's up with the Title "the"?
data.combined[which(titles == "the"),]
## only one person and her Title is "the Countess"

# we're going to re-map the titles to make it more concise; Lady, Dona, the Countess
# are all essentially the same Title (female nobility), Col., Capt., Major are all
# military men, etc.
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Major.", "Col.", "Capt.")] <- "Officer"
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."     # "Mlle." = Mademoiselle
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles == "Mme."] <- "Mrs."                   # "Mme." = Madame
table(titles)


# create new variable
data.combined$new.title <- as.factor(titles)

# plot and visualize new titles
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Survival Rates for new.title by Pclass")

## the tiny samples of nobility and/or specific Titles makes it easy for our model
## to overfit, we might want to reclassify some Titles into more general terms

# turning "Lady." into "Mrs."
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

# turning "Sir.", "Dr.", "Officer.", and "Rev." into "Mr."
index <- data.combined$new.title == "Sir." |
  data.combined$new.title == "Dr." |
  data.combined$new.title == "Officer." |
  data.combined$new.title == "Rev."
data.combined$new.title[index] <- "Mr."

# visualize
ggplot(data.combined[1:891,], aes(x = new.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Survival Rates for Collapsed new.title by Pclass")

# grab features for new decision tree model
features <- c("Pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]

# run CV and check results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

## accuracy rate for this model is 82.55% vs. 81.71%, pretty decent increase

# plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# let's dive in on 1st class "Mr."
indexes.first.mr <- which(data.combined$new.title == "Mr." & 
                            data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

# there's 1 female labeled "Mr."?
first.mr.df[first.mr.df$Sex == "female",]

# update new.title feature
indexes <- which(data.combined$new.title == "Mr." & data.combined$Sex == "female")
data.combined[indexes] <- "Mrs."

# any other gender slip-ups?
length(which(data.combined$Sex == "female" & (data.combined$new.title == "Master." |
               data.combined$new.title == "Mr.")))

# refresh data frame
indexes.first.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr,]

# let's look up the surviving 1st class Mr.'s
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])

# let's look at some of the high fares by ticket
indexes <- which(data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])

# visualize survival by 1st class Mr.'s
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = .5) +
  ggtitle("1st Class 'Mr.' Survival Rate by Fare")

# engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for(i in 1:length(tickets)){
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for(k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

# refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# visualize new features in first.mr.df
ggplot(first.mr.df[first.mr.df$Survived != "NONE",], aes(x = ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by ticket.part.size")

ggplot(first.mr.df[first.mr.df$Survived != "NONE",], aes(x = avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by avg.fare ")


# HYPOTHESIS: ticket.party.size is highly correlated with avg.fare
summary(data.combined$avg.fare)

# one missing value, take a look
data.combined[is.na(data.combined$avg.fare), ]

# we want to get records of similar passengers to Mr. Thomas Storey and get avg. fares
indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." & family.size == 1
                                     & Ticket != 3701))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$avg.fare)

# we'll use median because it's close to mean, but higher
data.combined[is.na(avg.fare), "avg.fare"] <- 7.840

# leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("ticket.party.size", "avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

# hypothesis refuted for all data
# finding correlation
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$avg.fare)

## ticket.party.size & avg.fare are highly uncorrelated - good news: potentially
## two new features to extract predictive power out of dataset

# how about for just 1st class all-up
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes], 
    postproc.data.combined$avg.fare[indexes])
## hypothesis refuted again

# OK let's see if our feature engineering has made a difference
features <- c("Pclass", "new.title", "family.size", "ticket.party.size", "avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# run cv and check results
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

## More accurate than original models, main difference being ticket.party.size replacing
## family.size. Instead of over-fitting specific numeric values for family size, this 
## model uses ticket.party.size and compares general values (ie. seeing if a party size
## was greater than or equal to 5)

## However, we still have a large discrepancy when it comes to generally predicting
## death for all "Mr.'s"


# submitting new cv to Kaggle.. subset test records and features
test.submit.df <- data.combined[892:1309, features]

# make predictions - "predict" function is needed to actually make predictive
# model - type = "class" gives us a 1(survived) or 0(perished) answer based on
# probabilities based on our data

rpart.3.preds <- predict(rpart.3.cv.1$finalModel, test.submit.df, type = "class")
table(rpart.3.preds)

# write out CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rpart.3.preds)

write.csv(submit.df, file = "RPART_SUB_20191004_1.csv", row.names = FALSE)

## submitted to Kaggle - got 80.382% accuracy

# now to make a prediction using Random Forest

features <- c("Pclass", "new.title", "ticket.party.size", "avg.fare")
rf.train.temp <- data.combined[1:891, features]

set.seed(1234)
rf.temp <- randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp

test.submit.df <- data.combined[892:1309, features]

# make predictions
rf.preds <- predict(rf.temp, test.submit.df)
table(rf.preds)

# write CSV file to submit to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.preds)

write.csv(submit.df, file = "RF_SUB_20191004_2.csv", row.names
          = FALSE)

## score was 79.904 - a little bit worse than last submission

# If we want to improve our model, a good place to start is focusing on where
# it gets things wrong.
#
# First let's look at the features using mutual information to get more insight.
# Our intuition is that the plot of our tree should align well to the
# definition of mutual information.
#
# Mutual Information of two random variables is a measure of mutual dependence
# between the variables. Even further, it measures the amount of information
# obtained about one variable through the other variable.
#
# install.packages("infotheo")
library(infotheo)

# mutual information is allowing us to compare each respective variable to
# rf.label which holds the survival rates - ie. we're comparing Pclass to
# survival rates, Sex to survival rates, etc. to see which ones are most
# closely related to survival

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
# all variables are discrete ie. integers except fare, so it discretizes
# which (not sure what it does but..) makes it usable for mutual information
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$Title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$avg.fare[1:891]))
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.first.char[1:891])
mutinformation(rf.label, data.combined$cabin.multiple[1:891])


# Okay, now let's leverage the tsne algorithm to create a 2-D representation
# of our data suitable for visualization starting with the folks our model
# gets correct very often - females and boys.
#
# Rtsne will create a 2D plot showing survival rates combining the 3 other
# main features: Pclass, ticket.party.size, avg.fare
#
# install.packages("Rtsne")
library(Rtsne)

most.correct <- data.combined[data.combined$new.title != "Mr.",]
indexes <- which(most.correct$Survived != "NONE")

tsne.1 <- Rtsne(most.correct[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2],
                 color = most.correct$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D Visualization of Features for Females and Boys")

# to get a baseline, let's use conditional mutual information on the tsne X and Y
# features for females and boys in 1st and 2nd class - the intuition here is that
# the combination of these features should be higher than any individual feature
# we looked at above
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))

# as one more comparison, we can leverage conditional mutual information
# using the top two features used in our tree plot - new.title & Pclass
condinformation(rf.label, data.combined[1:891, c("new.title", "Pclass")])


# Okay now let's look at adult males since this is the area we have the
# most potential for improving. Let's visualize with tsne
misters <- data.combined[data.combined$new.title == "Mr.",]
indexes <- which(misters$Survived != "NONE")

tsne.2 <- Rtsne(misters[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2],
                 color = misters$Survived[indexes])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D visualization of features for new.title of 'Mr.'")

# now to see conditional mutual information for tsne features for adult males
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))


#
# IDEA - How about creating tsne features for all the training data and
# using them in our model?
#
tsne.3 <- Rtsne(data.combined[, features], check_duplicates = FALSE)
ggplot(NULL, aes(x = tsne.3$Y[1:891, 1], y = tsne.3$Y[1:891, 2],
                 color = data.combined$Survived[1:891])) +
  geom_point() +
  labs(color = "Survived") +
  ggtitle("tsne 2D visualization for all Training Data")

# now conditional mutual information for all titles
condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891,]))

# add the tsne features to data frame for use in model building
data.combined$tsne.x <- tsne.3$Y[,1]
data.combined$tsne.y <- tsne.3$Y[,2]