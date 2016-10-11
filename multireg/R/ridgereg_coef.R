
coef.ridgereg<- function(object, ...){
  stopifnot(inherits(object, "ridgereg"))
  coef<- as.vector(object$Coefficients)
  names(coef)<- names(object$Coefficients)
  return(coef)
}

coef(ridgeobject)
