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
  summary<- summary(x)
  #sum<- sum$coefficients[,-1]
  #sigma<- sum$sigma
  #df<- sum$df
  #result<- list("call"=call,"Coefficients"=sum,"Sigma"=sigma, "DF"=df)
  return(summary)
}

