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


## ------------------------------------------------------------------------
lpRidgeReg <- list(type = "Regression",
                   library = "multireg",
                   loop = NULL) 

prm <- data.frame(parameter = "lambda",
                  class = "numeric",
                  label = "lambda")


lpRidgeReg$parameters<-prm

grid <- function (x, y, len = NULL, search = "grid")
{
  if (search == "grid") {
    out <- expand.grid(lambda = c(0, 10^seq(-1, -4, length = len - 1)))
  }
  else {
    out <- data.frame(lambda = 10^runif(len, min = -5, 1))
  }
  out
}

lpRidgeReg$grid <- grid

fitreg<-function(x,y,lambda,param,lev,last,classProbs,...){
  
  if(is.data.frame(x)){
    dat<- x
  }else{
    dat<- as.data.frame(x)
  } 
  dat$medv<- y

  frmla <- as.formula(paste(colnames(dat)[ncol(dat)], 
                            paste(colnames(dat)[1:(ncol(dat)-1)],
                                  sep = "", collapse = " + "), sep = " ~ "))
  
  model <- multireg::ridgereg(formula= frmla, data=dat, lambda= param)
  return(model)
}

lpRidgeReg$fit<-fitreg

pred <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
  predict(modelFit, newdata)
}

lpRidgeReg$predict <- pred


lpRidgeReg$prob <- list(NULL)


##

#trainridge <- train(y=training$medv,x = training,method =lpRidgeReg)
train(form= medv~., data=training,method ="ridge")

## ------------------------------------------------------------------------
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
trainridgecv <- train(form= medv~., data=training,method ="ridge",
                       trControl = fitControl)
trainridgecv

## ------------------------------------------------------------------------
testlm<-train(form=medv~., data=testing, method="lm")
testlm #RMSE 5,82 Rsq 0.621

testridge<-train(form=medv~., data= testing, method="ridge", lambda=0)
testridge #RMSE 5.49 Rsq 0.678
testridgecv <- train(form= medv~., data=testing,method ="ridge",
                       trControl = fitControl, lambda=0)
testridgecv # RMSE 5,13 Rsq 0.74



