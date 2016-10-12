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


## ---- echo=FALSE---------------------------------------------------------
data(iris)
train<- iris[1:75,]
test<- iris[76:150,]

pred_ridgeobject<- ridgereg(formula= Sepal.Length ~ Petal.Length + Petal.Width,
                       data=train, lambda=0)

new_pred<- predict(pred_ridgeobject, newdata=test)

head(new_pred)


