
ridgereg<- function(formula, data, lambda){
  
  # Create a model matrix X
  varX<- all.vars(formula[[3]])  #kolla vilka som ska va i model matrix
  form<- as.formula(paste("~",paste(varX,collapse="+"))) # gör formula till model matrix
  modmatX<- model.matrix(form, data=data) # model matrix X 
  
  #Normalisera xvar #
  X<- modmatX[,-1] 
  
  for(i in 1:ncol(X)){
    X[,i]<- (X[,i] - mean(X[,i])) / sqrt(var(X[,i]))
  }
  
  modmatX[,varX]<- X
   
  # skapa Y matris 
  y<- all.vars(formula)[1]   #hitta vilken dependent Y 
  which_y<- which(colnames(data) == y) # vilken columen är Y i data 
  Y<- as.matrix(data[,which_y]) # plocka ut Y from data
   
  tmodmatX<- t(modmatX) # transpose model matrix modmatX
    
  # beräkna ridge koefficienter #
  
  I<- diag(nrow(tmodmatX)) 
  
  B_hat<- solve((tmodmatX %*% modmatX) + (lambda*I)) %*% (tmodmatX %*% Y)
     
  # skapa identitets matris #
  
    
  #inv_tmodmatX<- solve(t_modmat + (lambda * I))
    
  #tmodmatXY<- tmodmatX %*% Y
    
  #B_hat<- inv_tmodmatX %*% tmodmatXY
  
  B_hat<- as.vector(B_hat)
  names(B_hat)<- colnames(modmatX)
  
  # beräkna fitted values #
  y_hat<- modmatX %*% B_hat
    
  ls<- list(Call= match.call(), Coefficients = B_hat, Fitted_values= as.vector(y_hat))
  class(ls)<- "ridgereg"
  return(ls)
}