#' Extract Beta Coefficients.
#' 
#' \code{coef} returns the estimated beta coefficients of the regression model calculated by the \code{\link{ridgereg}} function.
#' 
#' @param object An object of class \code{ridgereg} 
#' @param ... other arguments.
#' @return Coefficients extracted from the model \code{\link{ridgereg}} and returned as a named vector.
#'
#' @examples
#' x<- ridgereg(formula= Sepal.Width ~ Sepal.Length + Petal.Width, data= iris)
#' coef(x)
#' @export
coef.ridgereg<- function(object, ...){
  stopifnot(inherits(object, "ridgereg"))
  coef<- as.vector(object$Coefficients)
  names(coef)<- names(object$Coefficients)
  return(coef)
}
