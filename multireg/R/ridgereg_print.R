

print.ridgereg<- function(x, ...){
  stopifnot(inherits(x, "ridgereg"))
  call<-  x$Call
  coeff<- as.vector(x$Coefficients)
  names(coeff)<- names(x$Coefficients)
  result<- list(Call=call, Coefficients = coeff)
  return(result)
}

print(ridgeobject)

