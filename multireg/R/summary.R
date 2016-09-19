#' Extract Summary Results from linreg object.
#' 
#' \code{summary} returns the result summariest of the regression model fitted by the \code{\link{linreg}} function.
#' @param object An object of class \code{linreg} 
#' @param ... other arguments.
#' @return A list of summary result extracted from the model in \code{\link{linreg}}.
#'
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' summary(x)
#' @export


summary.linreg<- function(object, ...){
  coef<-object$coefficients
  stderr<-sqrt(diag(object$variance_of_regression_coefficent))
  tval<-object$tvalue
  pval<-object$pvalue
  deg<-object$df
  fort<-object$formula
  stdres<-sqrt(object$residual_variance)
  dataf<-data.frame("Coefficients"=coef, "Stderror"=stderr,"p-value"=pval,"t-value"=tval)
  result<-list("formula"=fort, "Coefficients"= dataf,"df"=deg,"standard error of residulals"=stdres)
  return(result)
}



