

predict.ridgereg<- function(object, ...){
  stopifnot(inherits(object, "ridgereg"))
  pred<- as.vector(object$Fitted_values)
  return(pred)
}

predict(ridgeobject)
