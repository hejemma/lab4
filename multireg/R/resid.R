#' Extract Residuals from linreg object.
#' 
#' \code{resid} returns the residuals of the regression model calculated by the \code{\link{linreg}} function.
#' 
#' @param object An object of class \code{linreg} 
#' @param ... other arguments.
#' @return A numeric vector with the residuals extracted from the model calculated by \code{\link{linreg}} function.
#' @import stats
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' resid(x)
#' @export


resid.linreg<- function(object, ...){
  res<- as.vector(object$residuals)
  return(res)
}


