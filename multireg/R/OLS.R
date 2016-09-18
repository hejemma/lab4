


## OBS, VERKLIGEN INTE FÄRDIGT.. 


#FÖR ATT TESTA ALLA MATRISUTRÄKNINGAR.. 
#  data(iris)
#  data<-iris
# formula<-(Petal.Length ~ Sepal.Length + Petal.Width)

#######################################################

#  linreg<- function(formula, data){

# skapa model matrix
langdX<-length(formula)  # number of parameters
varX<-all.vars(formula[[langdX]])  #kolla vilka som ska va i model matrix
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
sigma2_hat<- (t(e_hat) %*% e_hat) / df # sigma^2_ hat
sigma2_hat<- as.vector((t(e_hat) %*% e_hat) / df) # sigma^2_ hat as vector för att få den under att funka

#variance of reg. coef  ERRORRRR

var_B_hat<- sigma2_hat * inv_tmodmatX

# t values  ERRORRR 

t_beta<- B_hat/sqrt(var_B_hat)

# calculate p-values när den för t-values fungerar 

#pt()... 

#pull all statistics in a object
statobj<-()

### making obbject of class linreg 

class(statobj)<-"linreg"

}