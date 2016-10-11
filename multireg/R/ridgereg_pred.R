

predict.ridgereg<- function(object, ...){
  pred<- as.vector(object$Fitted_values)
  return(pred)
}

predict(ridgeobject)
