setwd("D:\\dropbox\\Dropbox\\科研工作\\L老婆毕业论文\\Data")
data<-read.csv('regg.csv')
library(car)
scatterplotMatrix(data.frame(data),spread=F,main='散点图图示')

attach(data)
fit<-lm(people~empl_B+income_B+air_B+farm+cata+road+healthcare+bx+edu)
detach(data)

#模型基本评价图

png(file="myplot.png", bg="transparent")
dev.off()
plot(fit)

library(car)
qqPlot(fit, labels=row.names(year), id.method="identify", simulate=TRUE, main="Q-Q Plot")

#检验多重共线性
library(car)
vif(fit)
sqrt(vif(fit))>2 #priblem?

#修正多重共线性的方法
#法1
#install.packages('leaps')
library(leaps)
leaps<-regsubsets(people~empl_B+income_B+air_B+farm+cata+road+healthcare+bx+edu,data=data,nbest=4)
plot(leaps,scale="adjr2")

#法2
library(MASS)
stepAIC(fit)

#主成分分析
#install.packages('psych')
library(psych)
attach(data)
mat<-as.matrix(data.frame(empl_B,income_B,air_B,farm,cata,road,healthcare,bx,edu))
rc<-principal(mat,nfactors=4,rotate='none',score=TRUE)
head(rc$scores)
mat1<-scale(mat)
#write.csv(mat1,'mat1.csv')
pc<-read.csv('pc.csv')
pc<-pc[,-1]
attach(pc)
pclm<-lm(people~PC1.+PC2.+PC3.)
summary(pclm)
pclm<-lm(people~PC1.+PC2.)
summary(pclm)

#简单线性预测for缺失数据
setwd("D:\\dropbox\\Dropbox\\科研工作\\L老婆毕业论文\\Data")
data1<-read.csv('11.csv')
library(car)
names(data1)<-c('year','aa')
lmlm<-lm(aa~year,data=data1)
lmlm

#上海
#主成分分析
#install.packages('psych')
setwd("D:\\dropbox\\Dropbox\\科研工作\\L老婆毕业论文\\Data")
data2<-read.csv('reg2.csv')
library(psych)
attach(data2)
mat<-as.matrix(data.frame(empl_S,income_S,air_S,farm,cata,road,healthcare,bx,edu))
rc<-principal(mat,nfactors=3,rotate='none',score=TRUE)
rc
head(rc$scores)
mat1<-scale(mat)
write.csv(mat1,'mat1.csv')
pc<-read.csv('pc.csv')
pc<-pc[,-1]
attach(pc)
pclm<-lm(people~PC1.+PC2.+PC3.)
summary(pclm)
pclm<-lm(people~PC1.+PC2.)
summary(pclm)

#天津
#主成分分析
setwd("D:\\dropbox\\Dropbox\\科研工作\\L老婆毕业论文\\Data")
data3<-read.csv('reg1.csv')
#install.packages('psych')
library(psych)
attach(data3)
mat<-as.matrix(data.frame(empl_T,income_T,air_T,farm,cata,road,healthcare,bx,edu))
rc<-principal(mat,nfactors=3,rotate='none',score=TRUE)
rc
head(rc$scores)
mat1<-scale(mat)
write.csv(mat1,'mat1.csv')
pc<-read.csv('pc.csv')
pc<-pc[,-1]
attach(pc)
pclm<-lm(people~PC1.+PC2.+PC3.)
summary(pclm)
pclm<-lm(people~PC1.+PC2.)
summary(pclm)