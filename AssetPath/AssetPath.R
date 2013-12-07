S0=50;      #initial price S0
mu=0.04;    #drift mu
sigma=0.3;  # volatility sigma
T=1;        #1 year
NSteps=252; 
NReps=30; 
SPaths = matrix(NA,NReps,NSteps+1);
SPaths[,1] = S0;
dt = T/NSteps
nudt = (mu-0.5*sigma^2)*dt
sidt = sigma*sqrt(dt)
x=seq(1:(NSteps+1))
colno=c(1,2,3,4,5,6,7,8,9,10)
for (i in 1:NReps){
  for (j in 1:NSteps){
    SPaths[i,j+1] = SPaths[i,j]*exp(nudt + sidt*rnorm(1))
  }
  if(i == 1) plot(x,SPaths[i,],"l", ylim=c(30,80),col =sample(colno,1))
  if (i != 1) lines(x,SPaths[i,],ylim=c(30,80),col =sample(colno,1))
  
 }
SPaths