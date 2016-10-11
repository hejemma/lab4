
stopFunction<- function(formula, data, lambda){
  
  stopifnot(class(formula) == "formula")
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(lambda))

}