
coef.ridgereg<- function(object, ...){
  coef<- as.vector(object$Coefficients)
  names(coef)<- names(object$Coefficients)
  return(coef)
}

coef(ridgeobject)
