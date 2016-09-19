#' Extract Beta Coefficients and equation if the model.
#' 
#' \code{print} returns the estimated beta coefficients and the equation of the regression model calculated by the \code{\link{linreg}} function.
#' @param x An object of class \code{linreg}
#' @param ... other arguments.
#' @return Coefficients and equations extracted from the model in \code{\link{linreg}} and returned as a named vector.
#'
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' print(x)
#' @export

print.linreg<- function(x, ...){
  call<-  x$formula
  coeff<- as.vector(x$coefficients)
  names(coeff)<- rownames(x$coefficients)
  result<- list(Formula =call, Coefficients = coeff)
  return(result)
}