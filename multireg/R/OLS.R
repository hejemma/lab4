#' Computing regression statistics using ordinary least squares.
#' 
#' The function calculates the regression coefficients, fitted values, residuals, 
#' degrees of freedom, residual variance, 
#' variance of regression coefficients, t-values and p-values for each coefficients.
#' @param formula A regression formula.
#' @param data A dataframe containing the variables used in formula.
#' @return An object of class linreg.
#' @examples
#' linreg(formula= Sepal.Width ~ Sepal.Length, data= iris)
#' linreg(formula = eruptions ~ waiting, data= faithful)
#' @export





#FÖR ATT TESTA ALLA MATRISUTRÄKNINGAR.. 
#  data(iris)
#  data<-iris
# formula<-(Petal.Length ~ Sepal.Length + Petal.Width)

#######################################################

linreg<- function(formula, data){

  # skapa model matrix
  langdX<-length(formula)  # number of parameters
  varX<-all.vars(formula[[3]])  #kolla vilka som ska va i model matrix
  formen<-as.formula(paste("~",paste(varX,collapse="+"))) # gör formula till model matrix
  modmatX<-model.matrix(formen, data=data) # model matrix X 
  
  # skapa Y matris 
  
  lillaY<-all.vars(formula)[1]   #hitta vilken dependent Y 
  vilken<-which( colnames(data)==lillaY ) # vilken columen är Y i data 
  Y<-as.matrix(data[,vilken]) # plocka ut Y from data
  
  tmodmatX<-t(modmatX) # transpose model matrix modmatX
  
  # compute regression coefficients B-hatt 
  
  
  inv_tmodmatX<- solve(tmodmatX %*% modmatX) #inverse of tmodmatX * modmatX
  
  tmodmatXY<-tmodmatX %*% Y #tmodmatX * Y 
  
  B_hat<- inv_tmodmatX %*% tmodmatXY # reg. coef B-hatt 
  
  # comput fitted values 
  
  y_hat<- modmatX %*% B_hat  # fitted values y hat X*B_hat
  
  # compute residuals 
  
  e_hat<- Y - y_hat # residuals Y- y_hat 
  
  # compute degress of freedom 
  
  n<-nrow(modmatX) # number of observations 
  
  df<-n-langdX # df 
  
  #compute residual variance 
  
  sigma2_hat<- as.vector((t(e_hat) %*% e_hat) / df) # sigma^2_ hat as vector för att få den under att funka
  
  
  #variance of reg. coef
  
  var_B_hat<- inv_tmodmatX* sigma2_hat
  
  #standard errors of parameter estimates 
  
  std_parest<- sqrt(diag(var_B_hat))
  
  # t values 
  t_beta<-B_hat/sqrt(diag(var_B_hat))
  
  
  # calculate p-values när den för t-values fungerar 
  
  pval<- 2 *( 1 - pt(abs(t_beta), df))
  
  formen<-formula
  #pull all statistics in a object
  statobj<-list("formula"=formen,"coefficients"=B_hat,"fitted"=y_hat,"residuals"=e_hat,
                "df"=df,"residual_variance"=sigma2_hat,
                "variance_of_regression_coefficent"=var_B_hat,
                "tvalue"=t_beta,"pvalue"=pval)
  
  ### making obbject of class linreg 
  
  class(statobj)<-"linreg"
  
  
  return(statobj)
}