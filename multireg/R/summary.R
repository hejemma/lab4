#' Extract Summary from linreg object.
#' 
#' \code{summary} returns the fitted values of the regression model calculated by the \code{\link{linreg}} function.
#' @param See \code{\link{linreg}}
#' @return Summary extracted from the model \code{x}.

#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' summary(x)
#' @export


summary.linreg<- function(x){
  coef<-x$coefficients
  stderr<-sqrt(diag(x$variance_of_regression_coefficent))
  tval<-x$tvalue
  pval<-x$pvalue
  deg<-x$df
  fort<-x$formula
  stdres<-sqrt(x$residual_variance)
  dataf<-data.frame("Coefficients"=coef, "Stderror"=stderr,"p-value"=pval,"t-value"=tval)
  result<-list(dataf,"df"=deg,"standard error of residulals"=stdres, "formula"=fort)
  return(result)
}



