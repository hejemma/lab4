#' Extract Fitted Values from linreg object.
#' 
#' \code{pred} returns the fitted values of the regression model calculated by the \code{\link{linreg}} function.
#' @param object An object of class \code{linreg} 
#' @param ... other arguments.
#' @return A numeric vector of fitted values extracted from the model in \code{\link{linreg}}.
#'
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' pred(x)
#' @export


predict.linreg<- function(object, ...){
  pred<- as.vector(object$fitted)
  return(pred)
}