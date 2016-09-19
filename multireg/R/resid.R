#' Extract Residuals from linreg object.
#' 
#' \code{resid} returns the residuals of the regression model calculated by the \code{\link{linreg}} function.
#' @param See \code{\link{linreg}}
#' @return residuals extracted from the model \code{x}.

#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' resid(x)
#' @export


resid.linreg<- function(x){
  resid<- as.vector(x$residuals)
  return(resid)
}


