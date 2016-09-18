resid<- function(x) UseMethod("resid")

resid.linreg<- function(x){
  resid<- as.vector(x$residuals)
  return(resid)
}

resid(linreg(formula=formel, data=iris))

