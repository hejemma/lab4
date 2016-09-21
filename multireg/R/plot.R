#' Plot residuals.
#' 
#' The function plots the residuals versus fitted values, and the standardized residuals
#' versus the fitted value 
#' @param x An object of class linreg.
#' @param ... other arguments.
#' @return A two in one plot over residuals versus fitted values.
#' @import ggplot2
#' @import gridExtra
#' @import graphics
#' @examples
#' x<- linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' plot(x)
#' @export


plot.linreg<-function(x, ...){
  plotdf<-data.frame(Fitted= x$fitted, Residuals= x$residuals)# select the relevant data and make df

  
  p<- ggplot(plotdf, aes(x=Fitted, y= Residuals))
  p<- p + geom_point() + ggtitle("Residual vs Fitted Plot")+theme_bw()
  p<- p + geom_smooth(color="red", method="lm", se=F) + 
    geom_hline(yintercept=0, col="black", linetype="dashed") # the dashed black line
  
  
  resstd<-sqrt(x$residual_variance) # std of residuals 
  std_res<-(plotdf$Residuals)/resstd  #standardized residuals 
  
  
  p2<- ggplot(plotdf, aes(x=Fitted, y=sqrt(abs(std_res)))) + geom_point() + geom_smooth(color="red", method="lm", se=F)+
    ggtitle("Scale Location") + ylab(expression(sqrt("|Standardized residuals")))+ theme_bw()
  grid.arrange(p,p2,ncol=1)
  
}
