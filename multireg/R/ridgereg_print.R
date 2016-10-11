

print.ridgereg<- function(x, ...){
  call<-  x$Call
  coeff<- as.vector(x$Coefficients)
  names(coeff)<- names(x$Coefficients)
  result<- list(Call=call, Coefficients = coeff)
  return(result)
}

print(ridgeobject)

