
library(MASS)
lm.ridge()
longley

names(longley)[1]<-"y"
#lm.ridge(y~ ., longley, lambda=seq(0,0.1,0.001))
#jämför med nedan
lm.ridge(y~ GNP + Unemployed + Armed.Forces + Population +Year + Employed, longley)


skalad<-apply((longley)[2:7], 2, function(x) (x-mean(x))/sd(x))
skalad<-cbind(longley[1],skalad)

formula= y~ GNP + Unemployed + Armed.Forces + Population +Year + Employed
data=skalad
#data=longley blir samma som lm.ridge 

# skapa model matrix
langdX<-length(formula)  # number of parameters
varX<-all.vars(formula[[3]])  #kolla vilka som ska va i model matrix
formen<-as.formula(paste("~",paste(varX,collapse="+"))) # gör formula till model matrix
modmatX<-model.matrix(formen, data=data) # model matrix X 

# skapa Y matris 

lillaY<-all.vars(formula)[1]   #hitta vilken dependent Y 
vilken<-which( colnames(data)==lillaY ) # vilken columen är Y i data 
Y<-as.matrix(data[,vilken]) # plocka ut Y from data

#lambda=seq(0,0.1,0.001)
bridge<- solve(t(modmatX)%*%modmatX) %*% t(modmatX)%*%Y
bridge # 