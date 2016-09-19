#' Plot residuals.
#' 
#' The function plots the residuals versus fitted values, and the standardized residuals
#' versus the fitted value 
#' @param argument An object of class linreg.
#' @return A two in one plot over residuals versus fitted values.
#' @examples
#' plot(argument=linreg)
#' @export






plot.linreg<-function(argument){
  plotdf<-data.frame(Fitted=argument$fitted,
                     Residuals=argument$residuals)# select the relevant data and make df
  #coefdf<-data.frame(Coef=argument$coefficients)  
  
  p<- ggplot(plotdf)
  p<- p + geom_point(aes(x=Fitted, y=Residuals)) + ggtitle("Residual vs Fitted Plot")+theme_bw()
  p<- p + stat_smooth(method="loess") + 
    geom_hline(yintercept=0, col="black", linetype="dashed") # the dashed black line
  
  
  resstd<-sqrt(argument$residual_variance) # std of residuals 
  std_res<-(plotdf$Residuals)/resstd  #standardized residuals 
  
  
  p2<- ggplot(plotdf, aes(x=Fitted, y=sqrt(abs(std_res)))) + geom_point() +
    ggtitle("Scale Location") + ylab(expression(sqrt("|Standardized residuals")))
  grid.arrange(p,p2,ncol=1)
  
}
# få dit den röda linjejäveln
