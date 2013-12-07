#DataMining 练习
#install.packages('DMwR')
library(DMwR)
setwd('d:\\dropbox\\Dropbox\\Apps\\R\\datamining\\课上练习')
GSPC <- as.xts(read.zoo('data.csv',header=T))
head(GSPC)

T.ind <- function(quotes,tgt.margin=0.025,n.days=10) {
  v <- apply(HLC(quotes),1,mean)

  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))
  for(x in 1:n.days) r[,x] <- Next(Delt(v,k=x),x)

  x <- apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}

candleChart(last(GSPC,'3 months'),theme='white',TA=NULL)
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind,col='red',legend='tgtRet')
addAvgPrice(on=1)
addT.ind()