#' Fitted values of the regression model.
#' 
#' Returning the fitted values of the regression model calculated by the \code{\link{linreg}} function.
#' @param See \code{\link{linreg}}
#' @return A named vector.
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' pred(x)
#' @export

pred.linreg<- function(x){
  fit<- as.vector(x$fitted)
  return(fit)
}


