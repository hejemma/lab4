
stopFunction<- function(formula, data, lambda){
  
  stopifnot(class(formula) == "formula")
  stopifnot(all.vars(formula) %in% names(data))
  stopifnot(is.data.frame(data))
  stopifnot(is.numeric(lambda))

}