#' Beta coefficients of the regression model.
#' 
#' Returning the beta estimates of the regression model calculated by the \code{\link{linreg}} function.
#' @param See \code{\link{linreg}}
#' @return A named vector.
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' coef(x)
#' @export


coef<- function(x) UseMethod("coef")

coef.linreg<- function(x){
  coef<- as.vector(x$coefficients)
  names(coef)<- rownames(x$coefficients)
  return(coef)
}



