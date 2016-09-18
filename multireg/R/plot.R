# ADD THE RED LINE

plot.linreg<-function(argument){
  plotdf<-data.frame(Fitted=argument$fitted,
                     Residuals=argument$residuals)# select the relevant data and make df
  #coefdf<-data.frame(Coef=argument$coefficients)  
  
  p<- ggplot(plotdf)
  p<- p + geom_point(aes(x=Fitted, y=Residuals)) + ggtitle("Residual vs Fitted Plot")+theme_bw()
  p<- p + stat_smooth(method="loess") + 
    geom_hline(yintercept=0, col="black", linetype="dashed") # the dashed black line
  p
}
