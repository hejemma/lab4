#' Extract Fitted Values from linreg object.
#' 
#' \code{pred} returns the fitted values of the regression model calculated by the \code{\link{linreg}} function.
#' @param x is an object of the class \code{linreg}
#' @return A numeric vector of fitted values extracted from the model in \code{\link{linreg}}.
#'
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' pred(x)
#' @export


predict.linreg<- function(object, ...){
  pred<- as.vector(x$fitted)
  return(pred)
}