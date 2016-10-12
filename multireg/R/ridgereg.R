
ridgereg<- function(formula, data, lambda=0){
  
  stopFunction(formula, data, lambda)
  
  # Create a model matrix X
  varX<- all.vars(formula[[3]])  #kolla vilka som ska va i model matrix
  form<- as.formula(paste("~",paste(varX,collapse="+"))) # gör formula till model matrix
  modmatX<- model.matrix(form, data=data) # model matrix X 
  
  #Normalisera xvar #
  X<- modmatX[,-1] 
  X<- apply(X, 2, function(x) (x-mean(x))/sd(x))
  modmatX[,varX]<- X
  
  # skapa Y matris 
  y<- all.vars(formula)[1]   #hitta vilken dependent Y 
  which_y<- which(colnames(data) == y) # vilken columen är Y i data 
  Y<- as.matrix(data[,which_y]) # plocka ut Y from data
   
  # transpose model matrix modmatX
  tmodmatX<- t(modmatX)
  
  # skapa identitets matris #
  I<- diag(nrow(tmodmatX))
  
  # beräkna ridge koefficienter #
  B_hat<- solve((tmodmatX %*% modmatX) + (lambda*I)) %*% (tmodmatX %*% Y)
  B_hat<- as.vector(B_hat)
  names(B_hat)<- colnames(modmatX)
  
  # beräkna fitted values #
  y_hat<- modmatX %*% B_hat
  formu1<-formula
    
  ls<- list(Call= match.call(), Coefficients = B_hat, Fitted_values= as.vector(y_hat), Formula=formu1)
  class(ls)<- "ridgereg"
  return(ls)
}