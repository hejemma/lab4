## ---- echo=FALSE---------------------------------------------------------
library(multireg)
data(iris)
ridgeobject<- ridgereg(formula= Sepal.Length ~ Petal.Length + Petal.Width,
                       data=iris, lambda=0)
ridgeobject

## ---- echo=FALSE---------------------------------------------------------
coef(ridgeobject)


## ---- echo=FALSE---------------------------------------------------------
print(ridgeobject)


## ------------------------------------------------------------------------
data(iris)
train<- iris[1:75,]
test<- iris[76:150,]

pred_ridgeobject<- ridgereg(formula= Sepal.Length ~ Petal.Length + Petal.Width,
                       data=train, lambda=0)

new_pred<- predict(pred_ridgeobject, newdata=test)

head(new_pred)


## ------------------------------------------------------------------------
library(mlbench)
library(leaps)
data("BostonHousing") #load data
library(caret)
inTrain <- createDataPartition(y = BostonHousing$medv,  p = .75, list = FALSE) #data partitioning

training <- BostonHousing[ inTrain,] #training set 
testing <- BostonHousing[-inTrain,] # test set 


## ------------------------------------------------------------------------

lmFit<-train(medv~., data = training, method="lm")# linear regression
lmFitForw<-train(medv~., data=training, method="leapForward")# forward selection

## ---- echo=FALSE---------------------------------------------------------
#linear regression
predicted<- predict(lmFit)
modvalues<- data.frame(obs=training$medv, pred=predicted) #df with observed and predicted
defaultSummary(modvalues)


## ---- echo=FALSE---------------------------------------------------------
#fast forward regression 
predictedFF<- predict(lmFitForw)
modvaluesFF<- data.frame(obs=training$medv, pred=predictedFF) #df with observed and predicted
defaultSummary(modvaluesFF)


