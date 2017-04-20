---
title: "ex2 try1"
author: "Amos Zamir"
date: "April 7, 2017"
output: html_document
---

# ex2

# Position in Leaderboard - 1257

![Leaderboard Image](https://github.com/zamiramos/ex2/blob/master/cforest_try1.PNG)

# Kaggle Username: zamiramos 

# Kaggle Profile Page: https://www.kaggle.com/zamiramos

## Environment setup

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd('D:/Code/Serve')
```

## Read Data

Read the train.csv and test.csv file into a dataframe. Use the parameter na.strings = "" to recognize empty strings as null values, otherwise these empty strings might raise errors when creating a model.
Bind togther the train and test before the 'feature engineering'.

```{r}
df <- read.csv("Titanic/train.csv",na.strings = "")
trainRows<-nrow(df)
test <-read.csv('Titanic/test.csv',na.strings = "")
test$Survived <- NA
df<-rbind(df,test)
```

Check the datatypes of the attributes using the *str* method. You should find two numeric features that must be converted into factors. Convert these two features to factors.

```{r}
str(df)
```
![str result Image](https://github.com/zamiramos/ex2/blob/master/images/7.PNG)

 
Fix numeric features that must be converted into factors.

```{r}
df$Survived<- as.factor(df$Survived)
df$Pclass<- as.factor(df$Pclass)
```

## Data Exploration

It is easier to explore factors and numeric features separately. Here we divide the features' names to numerics and factors:

```{R}
traindf<-head(df[-1], trainRows)

cols<- 1:dim(traindf)[2]
factors <- cols[sapply(traindf,is.factor)]
numerics <- cols[!sapply(traindf,is.factor)]
```

We now tide the data two times: the first is for categorial data and the second for numeric data.
```{r}
#install.packages("tidyr")
library(tidyr)
df_tidy_factors<-gather(traindf[,factors],"feature","value",-1)
df_tidy_numerics<-gather(cbind(Survived=traindf[,1],traindf[,numerics]),"feature","value",-1)
```
Finally, we can plot. The first plot describes only categorical features (factors). 
Notice that the *scales* parameter was set to "free" to enable a suitable scaling for each facet (otherwise it is hard to view some of the facets, that need much smaller scales). We use the *facet_grid* that accepts a *scales* parameter.
```{r}
#install.packages("ggplot2")
library(ggplot2)
qplot(x=value,data=df_tidy_factors,fill=Survived) + facet_grid(~feature,scales="free")
```
![Data Exploration Factor Image](https://github.com/zamiramos/ex2/blob/master/images/1.jpg)

It looks like Cabin, Name, Ticket has many levels and needs to be processed for getting more valuable features.


One more plot for numeric features:
```{r}
qplot(x=value,data=df_tidy_numerics,fill=Survived) + facet_grid(~feature,scales="free")
```
![Data Exploration Numeric Image](https://github.com/zamiramos/ex2/blob/master/images/2.jpg)

It certainly looks luck there are more chances to survive in certain levels of almost each feature.

## Feature engineering
```{r TicketPrefix}
df$TicketPrefix <- mapply(function(x) {strsplit(x, '\\s+')[[1]]}, as.character(df$Ticket))
df$TicketPrefix <- mapply(function(x) {ifelse(length(x)>1, x[1], NA)}, df$TicketPrefix)
df$TicketPrefix <- mapply(function(x) {gsub('[./,]','', x)}, df$TicketPrefix)
df$TicketPrefix <- mapply(function(x) {toupper(x)}, df$TicketPrefix)
df$TicketPrefix <- as.factor(df$TicketPrefix)
table(df$TicketPrefix)
```

![str result Image](https://github.com/zamiramos/ex2/blob/master/images/8.PNG)


Sir - gather all man respectful titles
Lady - gather all women respectful titles
Mlle - In english equals to Miss
Mme - In english equals to Ms

Seperate the PersonalTitles from the passenger names.
```{r PersonalTitles}
df$PersonalTitles <- mapply(function(x) {strsplit(x, '[,.]')[[1]][2]}, as.character(df$Name))
df$PersonalTitles[df$PersonalTitles %in% c(' Capt',' Col', ' Don', ' Major', ' Sir')] <- ' Sir'
df$PersonalTitles[df$PersonalTitles %in% c(' Jonkheer',' Dona', ' Lady', ' the Countess')] <- ' Lady'
df$PersonalTitles[df$PersonalTitles %in% c(' Mlle')] <- ' Miss'
df$PersonalTitles[df$PersonalTitles %in% c(' Mme')] <- ' Ms'
df$PersonalTitles<-as.factor(df$PersonalTitles)
table(df$PersonalTitles)
```
![str result Image](https://github.com/zamiramos/ex2/blob/master/images/9.PNG)


Seperate the surname from the passenger names
```{r SurName}
SurName <- mapply(function(x) {strsplit(x, '[,.]')[[1]][1]}, as.character(df$Name))
```


sibSp - number of siblings / spouses aboard the Titanic
parch - number of parents / children aboard the Titanic
FamilySize = sibSp + parch + 1
```{r FamilySize}
df$FamilySize <- mapply(function(sibSp, parch) { sibSp + parch + 1}, df$SibSp, df$Parch)
table(df$FamilySize)
```
![str result Image](https://github.com/zamiramos/ex2/blob/master/images/10.PNG)

Combine FamilySize and SurName to new feature
```{r FamilySizeSurName}
df$FamilySizeSurName <- mapply(function(familyS, surN) { paste(as.character(familyS), as.character(surN), sep='')}, df$FamilySize, SurName)
FamilySizeSurNameCount<-as.data.frame(table(df$FamilySizeSurName))

df$FamilySizeSurName <- mapply(function(familySS) { 
  (FamilySizeSurNameCount[which(FamilySizeSurNameCount$Var1 == familySS),]$Freq -mean(FamilySizeSurNameCount$Freq)) /sqrt(var(FamilySizeSurNameCount$Freq))
  }, df$FamilySizeSurName)
# it seems that big families most likely not survived
#plot(df$FamilySizeSurName, df$Survived)
```

The cabin room number can be meaningful.
```{r CabinMinRoomNumber}
df$CabinMinRoomNumber<-mapply(function(x)(gsub('[a-zA-Z]', '', x)), df$Cabin)
df$CabinMinRoomNumber <- mapply(function(x) {strsplit(x, '\\s+' )[[1]][1]}, as.character(df$CabinMinRoomNumber))
df$CabinMinRoomNumber <- as.integer(df$CabinMinRoomNumber)
table(df$CabinMinRoomNumber)
```
![str result Image](https://github.com/zamiramos/ex2/blob/master/images/11.PNG)

  
The cabin level can be meaningful.
```{r CabinLevel}
df$CabinLevel<-mapply(function(x)(gsub('[^a-zA-Z]', '', x)), df$Cabin)
df$CabinLevel <- mapply(function(x) {substr(x,1,1)}, as.character(df$CabinLevel))
df$CabinLevel <- as.factor(df$CabinLevel)
table(df$CabinLevel)
```

![str result Image](https://github.com/zamiramos/ex2/blob/master/images/12.PNG)

### Complete NA's values
Lets run summary to check how much NA's we have
```{r}
summary(df)
```
![str result Image](https://github.com/zamiramos/ex2/blob/master/images/13.PNG)

we can see that 'Age' have 263 missing values and 'Fare' have one and etc..
Lets complete these values using rpart algorithm:

1.Predict Age Column using 'anova' method because it continues value
```{r}
library('rpart')
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + PersonalTitles + FamilySize,
                  data=df[!is.na(df$Age),], 
                  method="anova")
df$Age[is.na(df$Age)] <- predict(Agefit, df[is.na(df$Age),])
```

2.Predict Ticket coulumn
```{r}
TicketPrefixfit <- rpart(TicketPrefix ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked + PersonalTitles + FamilySize,
                  data=df[!is.na(df$TicketPrefix),], 
                  method="class")
df$TicketPrefix[is.na(df$TicketPrefix)] <- predict(TicketPrefixfit, df[is.na(df$TicketPrefix),],type = "class")
```

3.Predict Fare coulmn
```{r}
Farefit <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Age + Embarked + PersonalTitles + FamilySize,
                  data=df[!is.na(df$Fare),], 
                  method="anova")
df$Fare[is.na(df$Fare)] <- predict(Farefit, df[is.na(df$Fare),])
```

4.Predict Embarked coulomn
```{r}
Embarkedfit <- rpart(Embarked ~ Pclass + Age + Sex + SibSp + Parch + Fare + PersonalTitles + FamilySize,
                  data=df[!is.na(df$Embarked),], 
                  method="class")
df$Embarked[is.na(df$Embarked)] <- predict(Embarkedfit, df[is.na(df$Embarked),],type = "class")
```

5.Predict CabinMinRoomNumber column
```{r}
CabinMinRoomNumberfit <- rpart(CabinMinRoomNumber ~ Pclass + Sex + SibSp + Parch + + Fare+ Age + Embarked + PersonalTitles + FamilySize,
                  data=df[!is.na(df$CabinMinRoomNumber),], 
                  method="anova")
df$CabinMinRoomNumber[is.na(df$CabinMinRoomNumber)] <- predict(CabinMinRoomNumberfit, df[is.na(df$CabinMinRoomNumber),])
```


6.Predict Cabin Level column
```{r}
CabinLevelfit <- rpart(CabinLevel ~ Pclass + Sex + Fare + Age + PersonalTitles,
                  data=df[!is.na(df$CabinLevel),], 
                  method="class")
df$CabinLevel[is.na(df$CabinLevel)] <- predict(CabinLevelfit, df[is.na(df$CabinLevel),],type = "class")
```

### Normalize fare data
```{r}
df$Fare<-mapply(function(fare){(fare - mean(df$Fare))/sqrt(var(df$Fare))}, df$Fare)
```

## Removes unnecessary columns

```{r}
dfBackup<-df
df <- df[,-c(1,4, 9, 11)]
```

## split the data

Split the data back to test and train
```{r}
traindf<-head(df, trainRows)
testdf<-tail(df, -trainRows)[-1]
```

# Submissions 

## try1 - using cforest

### Feature Engineriing
In the section of feature engineering above.

### Algorithm Description

cforest - An implementation of the random forest and bagging ensemble algorithms utilizing conditional inference trees as base learners.

Function cforest_unbiased returns the settings suggested for the construction of unbiased random
forests (teststat = "quad", testtype = "Univ", replace = FALSE) by Strobl et al.
(2007) and is the default since version 0.9-90

### Algorithm Calibration

#### ctree Statistics
```{r}
library(party)
library(caret)
set.seed(123)

#Fare + PersonalTitles
#TicketPrefix + FamilySize + PersonalTitles + FamilySizeSurName + CabinMinRoomNumber
cforestfit1<-cforest(Survived~ Pclass + Sex + Age + Embarked + TicketPrefix + FamilySize + PersonalTitles +                        FamilySizeSurName + CabinMinRoomNumber,
                   data = traindf,
                   controls=cforest_unbiased(ntree=400, mtry=3))

cforestfit2<-cforest(Survived~ Pclass + Sex + Age + Embarked + TicketPrefix + FamilySize + PersonalTitles +                        FamilySizeSurName + CabinMinRoomNumber,
                   data = traindf,
                   controls=cforest_unbiased(ntree=800, mtry=3))

cforestfit3<-cforest(Survived~ Pclass + Sex + Age + Embarked + TicketPrefix + FamilySize + PersonalTitles +                        FamilySizeSurName + CabinMinRoomNumber,
                   data = traindf,
                   controls=cforest_unbiased(ntree=1200, mtry=3))

cforestfit4<-cforest(Survived~ Pclass + Sex + Age + Embarked + TicketPrefix + FamilySize + PersonalTitles +                        FamilySizeSurName + CabinMinRoomNumber,
                   data = traindf,
                   controls=cforest_unbiased(ntree=2000, mtry=3))

cforestfit5<-cforest(Survived~ Pclass + Sex + Age + Embarked + TicketPrefix + FamilySize + PersonalTitles +                        FamilySizeSurName + CabinMinRoomNumber,
                   data = traindf,
                   controls=cforest_unbiased(ntree=3000, mtry=3))

cforestfit <- c(cforestfit1, cforestfit2, cforestfit3, cforestfit4, cforestfit5)

cforestStatsArray<-sapply(cforestfit, function(x){ cforestStats(x) })

cforestStatsArray
```

![str result Image](https://github.com/zamiramos/ex2/blob/master/images/14.PNG)

It seems that ntree=2000 is the best.

```{r}
cforestfit <- cforestfit4
```

#### Feature Importance
```{r}
cforest_importance <- varimp(cforestfit)
dotchart(cforest_importance[order(cforest_importance)])
```
![Feature Importance Image](https://github.com/zamiramos/ex2/blob/master/images/3.jpg)

#### predection
```{r}
Prediction <- predict(cforestfit, testdf, OOB=TRUE, type = "response")
```

write to file
```{r}
res1<-as.numeric(Prediction)
res1[res1==1]<-0
res1[res1==2]<-1
res <- cbind(PassengerId=test$PassengerId,Survived=res1)
write.csv(res,file="Titanic/try1.csv",row.names = F)
```


### Shortcut to submitted file

[link to submitted file!](https://github.com/zamiramos/ex2/blob/master/cforest_try1.csv)

### Screenshot of Kaggle Score

![Leaderboard Image](https://github.com/zamiramos/ex2/blob/master/cforest_try1.PNG)


## try2 - using rpart

### Feature Engineriing
In the section of feature engineering above.

### Algorithm Description
The rpart package found in the R tool can be used for classification by decision trees and can also be used to generate regression trees. Recursive partitioning is a fundamental tool in data mining. It helps us explore the structure of a set of data, while developing easy to visualize decision rules for predicting a categorical (classification tree) or continuous (regression tree) outcome. The rpart programs build classification or regression models of a very general structure using a two stage procedure; the resulting models can be represented as binary trees. The tree is built by the following process: first the single variable is found which best splits the data into two groups ('best' will be defined later). The data is separated, and then this process is applied separately to each sub-group, and so on recursively until the subgroups either reach a minimum size (5 for this data) or until no improvement can be made. The resultant model is, with certainty, too complex, and the question arises as it does with all stepwise procedures of when to stop. The second stage of the procedure consists of using cross-validation to trim back the full tree.

### Algorithm Calibration

#### rpart Statistics
```{r}
library(rpart)
set.seed(123)

trcontrol <- trainControl(method="cv", number=10, repeats=3)

rpartfit <- train(
        Survived~., data=traindf, method="rpart", trControl=trcontrol, 
          tuneGrid = expand.grid(
            cp = c(0.01,0.005,0.0005)
          ))


plot(rpartfit)
```
![rpart accuracy Image](https://github.com/zamiramos/ex2/blob/master/images/4.jpg)

it seems like 0.005 give the best acuuracy.

#### Feature Importance
```{r}
varImp(rpartfit)
```
![str result Image](https://github.com/zamiramos/ex2/blob/master/images/15.PNG)


#### predection
```{r}
Prediction <- predict(rpartfit, testdf)
```

write to file
```{r}
res1<-as.numeric(Prediction)
res1[res1==1]<-0
res1[res1==2]<-1
res <- cbind(PassengerId=test$PassengerId,Survived=res1)
write.csv(res,file="Titanic/try2.csv",row.names = F)
```


### Shortcut to submitted file

[link to submitted file!](https://github.com/zamiramos/ex2/blob/master/rpart_try2.csv)

### Screenshot of Kaggle Score

![Leaderboard Image](https://github.com/zamiramos/ex2/blob/master/rpart_try2.PNG)



## try3 - using gbm - Stochastic Gradient Boosting

### Feature Engineriing
In the section of feature engineering above.

### Algorithm Description
Gradient boosting is a machine learning technique for regression and classification problems, which produces a prediction model in the form of an ensemble of weak prediction models, typically decision trees. It builds the model in a stage-wise fashion like other boosting methods do, and it generalizes them by allowing optimization of an arbitrary differentiable loss function.

### Algorithm Calibration

#### gbm Statistics
```{r}
library(stats)
set.seed(123)

trcontrol <- trainControl(method="cv", number=10, repeats=3)

gbmfit <- train(
        Survived~., data=traindf, method="gbm", trControl=trcontrol, 
          tuneGrid = expand.grid(
            n.trees = c(1000,2000), 
            interaction.depth = c(5,10,20),
            shrinkage = c(0.001, 0.01, 0.1),
            n.minobsinnode = c(10, 20, 30)
          ),
        verbose = FALSE)


plot(gbmfit)
```
![Plot gbmfit Image](https://github.com/zamiramos/ex2/blob/master/images/5.jpg)

We can see that the best tuning is shrinkage = 0.01 and interaction.depth is 10. n.minobsinnode = 30

lets test more ntree options:

```{r}
library(stats)
set.seed(123)

trcontrol <- trainControl(method="cv", number=10, repeats=3)

gbmfit <- train(
        Survived~Sex + Age + Embarked + TicketPrefix + FamilySize + PersonalTitles + FamilySizeSurName + CabinMinRoomNumber, data=traindf, method="gbm", trControl=trcontrol, 
          tuneGrid = expand.grid(
            n.trees = c(300, 500, 1000, 1500,2000), 
            interaction.depth = 10,
            shrinkage = 0.01,
            n.minobsinnode = 30
          ),
        verbose = FALSE)


plot(gbmfit)
```
![Plot gbmfit Image](https://github.com/zamiramos/ex2/blob/master/images/6.jpg)

great ntree = 500 is the best score.


### Predict
```{r}
new_pred<- predict(gbmfit,newdata = testdf)
new_pred<-sapply(new_pred, function(x){ as.character(x) == "1" })
res <- cbind(PassengerId=test$PassengerId,Survived=new_pred)
write.csv(res,file="Titanic/try3.csv",row.names = F)
```

### Shortcut to submitted file

[link to submitted file!](https://github.com/zamiramos/ex2/blob/master/gbm_try3.csv)

### Screenshot of Kaggle Score

![Leaderboard Image](https://github.com/zamiramos/ex2/blob/master/gbm_try3.PNG)


## try4 - Using Caret Stacking

### Feature Engineriing
In the section of feature engineering above.

### Algorithm Description
We will use in ensemble of the 3 models (above- try1,try2,try3):
1. cforest - An implementation of the random forest and bagging ensemble algorithms utilizing conditional inference trees as base learners.
2. rpart - Decision trees.
3. gbm - Stochastic Gradient Boosting.

and RandomForest as the stacking algorithm.

### Algorithm Calibration
We already calibrated the parameters of each algorithm separately.

Let's look that there is no high colleration between them:

```{r}
library(caret)
library(caretEnsemble)
library(party)
set.seed(123)
control <- trainControl(method="cv", number=10, savePredictions='final', classProbs=TRUE)
models <- caretList(
  make.names(Survived)~., 
  data=traindf,
  metric='Accuracy', 
  trControl=control,
  tuneList = list(
    cforest = caretModelSpec(
      method = "cforest",
      controls = cforest_unbiased(ntree=2000, mtry=3)
    ),
    gbm = caretModelSpec(
      method = "gbm",
      tuneGrid = expand.grid(
        n.trees = 1000, 
        interaction.depth = 10,
        shrinkage = 0.01,
        n.minobsinnode = 30
        ),
      verbose = FALSE
    ),
    rpart = caretModelSpec(
      method = "rpart",
      tuneGrid = expand.grid(
          cp = 0.005
        )
    )
  )
  )
results <- resamples(models)
summary(results)
modelCor(results)
```
![str result Image](https://github.com/zamiramos/ex2/blob/master/images/16.PNG)
![str result Image](https://github.com/zamiramos/ex2/blob/master/images/17.PNG)


We can see that there is no high correlation (<75) between them and we can continue to build one model from all the models:

```{r}
set.seed(123)
stackControl <- trainControl(method="cv", number=10, savePredictions='final', classProbs=TRUE)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl, tuneGrid = 
                         expand.grid(mtry = c(2, 3, 6, 9)))
print(stack.rf)
```
![str result Image](https://github.com/zamiramos/ex2/blob/master/images/18.PNG)

### Predict Test File and Write results

```{r}
new_pred<- predict(stack.rf,newdata = testdf)
length(new_pred)
new_pred<-sapply(new_pred, function(x){ as.character(x) == "X1" })
```

Write the *PassengerId* and *Survived* attributes to a csv file and submit this file to kaggle's competition 

```{r}
res <- cbind(PassengerId=test$PassengerId,Survived=new_pred)
write.csv(res,file="Titanic/try4.csv",row.names = F)
```


### Shortcut to submitted file

[link to submitted file!](https://github.com/zamiramos/ex2/blob/master/rf_ensemble_try4.csv)

### Screenshot of Kaggle Score

![Leaderboard Image](https://github.com/zamiramos/ex2/blob/master/rf_ensemble_try4.PNG)
