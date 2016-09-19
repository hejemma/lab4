#' Extract Residuals from linreg object.
#' 
#' \code{resid} returns the residuals of the regression model calculated by the \code{\link{linreg}} function.
#' 
#' @param x is an object of the class \code{linreg}.
#' 
#' @return A numeric vector with the residuals extracted from the model calculated by \code{\link{linreg}} function.
#'
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' resid(x)
#' @export


resid.linreg<- function(x){
  res<- as.vector(x$residuals)
  return(res)
}


