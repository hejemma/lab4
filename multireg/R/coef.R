#' Extract Beta Coefficients.
#' 
#' \code{coef} returns the estimated beta coefficients of the regression model calculated by the \code{\link{linreg}} function.
#' 
#' @param object An object of class \code{linreg} 
#' @param ... other arguments.
#' @return Coefficients extracted from the model \code{\link{linreg}} and returned as a named vector.
#'
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' coef(x)
#' @export
  
coef.linreg<- function(object, ...){
  coef<- as.vector(object$coefficients)
  names(coef)<- rownames(object$coefficients)
  return(coef)
}

