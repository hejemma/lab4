coef<- function(x) UseMethod("coef")

coef.linreg<- function(x){
  coef<- as.vector(x$coefficients)
  names(coef)<- rownames(x$coefficients)
  return(coef)
}


