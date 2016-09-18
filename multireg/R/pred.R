pred<- function(x) UseMethod("pred")

pred.linreg<- function(x){
  pred<- as.vector(x$"fitted values")
  return(pred)
}


