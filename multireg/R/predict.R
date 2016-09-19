#' Extract Fitted Values from linreg object.
#' 
#' \code{pred} returns the fitted values of the regression model calculated by the \code{\link{linreg}} function.
#' @param See \code{\link{linreg}}
#' @return Fitted values extracted from the model \code{x}.

#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' pred(x)
#' @export


predict.linreg<- function(x){
  pred<- as.vector(x$fitted)
  return(pred)
}