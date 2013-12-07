setwd("D:\\dropbox\\Dropbox\\科研工作\\L老婆毕业论文\\Data")
library(foreign)
library(Hmisc)
#migind<-read.dta("migind.dta")
#mighh<-read.dta("mighh.dta")
#data<-read.csv('1.csv') #会很慢
#write.csv(migind,'mgind.csv')

migind<-read.csv('datamigind.csv')

attach(migind);names(migind)
#attach(mighh);names(mighh)

newmigind<-na.omit(migind)
describe(newmigind)

X<-newmigind

prcomp(X,scale=T)->pr;#意思是利用相关矩阵计算主成分
summary(pr);#可以看出特征值和特征向量
predict(pr)#主成分

#或者
princomp(X,cor=T)->pr;# 与上面的一样
summary(pr,loadings=T);#loadings only for princomp
predict(pr);
screeplot(pr);#可以看出主成分的方差，
biplot(pr);#我不太清楚



pricom=function(x){ 
x=scale(x); 
cor=cor(x); 
eig=eigen(cor); 
cm=sweep(eig$ve,2,sqrt(eig$va),"*"); 
par(mfrow=c(1,2)); 
plot(eig$va,type="b",pch=22,col="red",ylab="EigenValue", xlab="Component Numbers",main="Scree Plot"); 
plot(cm[,1],cm[,2],pch=22,ylab="Component2",xlab="Component1",col="green", 
ylim=c(-1,1),xlim=c(-1,1),main="Component Plot"); 
abline(h=0,v=0,col="blue"); 
rownames(cm)=colnames(x); 
colnames(cm)=paste("Comp",c(1:dim(x)[2])); 
write.csv(cm,"D://Component Mtrix.csv",row.names=T) 
cm 
} 

scores=function(x){ 
x=scale(x); 
cor=cor(x); 
eig=eigen(cor); 
cm=sweep(eig$ve,2,sqrt(eig$va),"*"); 
cm2=sweep(cm,2,eig$va/sum(eig$va),"*"); 
scores=x%*%cm2; 
write.csv(scores,"D://scores.csv",row.names=T) 
scores; 
} 

pricom(X)