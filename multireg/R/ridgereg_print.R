#' Extract Beta Coefficients and equation if the model.
#' 
#' \code{print} returns the estimated beta coefficients and the equation of the regression model calculated by the \code{\link{ridgereg}} function.
#' @param x An object of class \code{ridgereg}
#' @param ... other arguments.
#' @return Coefficients and equations extracted from the model in \code{\link{ridgereg}} and returned as a named vector.
#'
#' @examples
#' x<- ridgereg(formula= Sepal.Width ~ Sepal.Length + Petal.Width, data= iris)
#' print(x)
#' @export

print.ridgereg<- function(x, ...){
  stopifnot(inherits(x, "ridgereg"))
  call<-  x$Call
  coeff<- as.vector(x$Coefficients)
  names(coeff)<- names(x$Coefficients)
  result<- list(Call=call, Coefficients = coeff)
  return(result)
}



