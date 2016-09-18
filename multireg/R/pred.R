#' Fitted values of the regression model.
#' 
#' Returning the fitted values of the regression model calculated by the \code{\link{linreg}} function.
#' @param See \code{\link{linreg}}
#' @return A named vector.
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' coef(x)
#' @export


pred<- function(x) UseMethod("pred")

pred.linreg<- function(x){
  pred<- as.vector(x$"fitted values")
  return(pred)
}


