#' Extract Summary Results from linreg object.
#' 
#' \code{summary} returns the result summariest of the regression model fitted by the \code{\link{linreg}} function.
#' @param x is an object of the class \code{linreg}
#' @return A list of summary result extracted from the model in \code{\link{linreg}}.
#'
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' summary(x)
#' @export


summary.linreg<- function(object, ...){
  coef<-x$coefficients
  stderr<-sqrt(diag(x$variance_of_regression_coefficent))
  tval<-x$tvalue
  pval<-x$pvalue
  deg<-x$df
  fort<-x$formula
  stdres<-sqrt(x$residual_variance)
  dataf<-data.frame("Coefficients"=coef, "Stderror"=stderr,"p-value"=pval,"t-value"=tval)
  result<-list("formula"=fort, "Coefficients"= dataf,"df"=deg,"standard error of residulals"=stdres)
  return(result)
}



