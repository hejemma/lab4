## ------------------------------------------------------------------------
library(multireg)
linreg_object<-linreg(formula = Sepal.Width ~ Sepal.Length, data= iris)
names(linreg_object)

## ---- eval=FALSE---------------------------------------------------------
#  coef(linreg_object) # prints the coefficients
#  predict(linreg_object) # prints the fitted values
#  resid(linreg_object)# # prints the resiudals
#  print(linreg_object) # prints the equation and the beta coefficients
#  summary(linreg_object) # an output similar to the summary() statement of lm-objects.

## ---- echo=FALSE---------------------------------------------------------
summary(linreg_object)


## ---- fig.width=7, fig.height=4------------------------------------------
library(ggplot2)
library(gridExtra)
plot(linreg_object)


