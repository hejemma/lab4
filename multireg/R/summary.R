summary<- function(x) UseMethod("summary")

summary.linreg<- function(x){
  summary<- summary(x)
  #sum<- sum$coefficients[,-1]
  #sigma<- sum$sigma
  #df<- sum$df
  #result<- list("call"=call,"Coefficients"=sum,"Sigma"=sigma, "DF"=df)
  return(summary)
}

