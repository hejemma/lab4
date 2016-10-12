#' Extract Fitted Values from linreg object.
#' 
#' \code{pred} returns the fitted values of the regression model calculated by the \code{\link{ridgereg}} function.
#' @param object An object of class \code{ridgereg} 
#' @param newdata If another data set with same variables is wished to use for prediction
#' @param ... other arguments like .
#' @return A numeric vector of fitted values extracted from the model in \code{\link{ridgereg}}.
#'
#' @examples
#' x<- ridgereg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' predict(x)
#' @export



predict.ridgereg<- function(object, newdata,...){
  stopifnot(inherits(object, "ridgereg"))
  
  if(missing(newdata)) {
    predd<-as.vector(object$Fitted_values)
  }else{
    pred1<-ridgereg(formula=object$Formula, data=newdata)
    predd<-pred1$Fitted_values
  }
  return(predd)
  
  }
