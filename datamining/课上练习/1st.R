setwd("d:\\dropbox\\Dropbox\\Apps\\R\\datamining\\数据挖掘\\数据挖掘数据")
data<-read.csv('Housing.csv')
names(data)
names(data)<-c('crim','zn','indus','chas','nox','rm','age','dis','rad','tax','ptratio','b','lstat','medv')
attach(data)
cor(data)
plot(density(crim))
plot(density(medv[is.na(medv)==F]))

library(Hmisc)
describe(data)

library(car)
scatterplotMatrix(data.frame(data),spread=F,main='散点图图示')
qqnorm(medv)
qqnorm(rm)

lm1<-lm(crim~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat+medv)
lm2<-lm(medv~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat+crim)
summary(lm1)
summary(lm2)

#install.packages('leaps')
#全子集回归
library(leaps)
leaps<-regsubsets(crim~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat+medv,data=data,nbest=4)
plot(leaps,scale="adjr2")

leaps<-regsubsets(medv~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat+crim,data=data,nbest=4)
plot(leaps,scale="adjr2")



#运用logistic回归，以chas为被解释变量
fit1<-glm(chas~zn+indus+crim+nox+rm+age+dis+rad+tax+ptratio+b+lstat+medv,family=binomial,data=data)
summary(fit1)

###K均值聚类方法
#先确定类数
library(fpc)
x<-c()
for (i in 2:10){
#K聚类结果存于result变量
result <- kmeans(data,i)
#求出聚类评价统计量
stats=cluster.stats(dist(data), result$cluster)
#将结果存入X
x[i]=stats$avg.silwidth
}
x
#说明分成两类好
julei1 <- kmeans(data,centers=2,nstart=10)


dist.e=dist(data,method='euclidean')  #计算距离


###距离聚类方法
dist.e=dist(data,method='euclidean')  #计算距离
heatmap(as.matrix(dist.e),labRow = F, labCol = F) #画出热力阶图

#然后使用hclust函数建立聚类模型，结果存在model1变量中，其中ward参数是将类间距离计算方法设置为离差平方和法。
#使用plot(model1)可以绘制出聚类树图。如果我们希望将类别设为3类，可以使用cutree函数提取每个样本所属的类别。 

model1=hclust(dist.e,method='ward')
result=cutree(model1,k=3)
#plot(result)

#为了显示聚类的效果，我们可以结合多维标度和聚类的结果。先将数据用MDS进行降维，然后以不同的的形状表示原本的分类，用不同的颜色来表示聚类的结果。
#可以看到setose品种聚类很成功，但有一些virginica品种的花被错误和virginica品种聚类到一起。 

mds=cmdscale(dist.e,k=2,eig=T)
x = mds$points[,1]
y = mds$points[,2]
library(ggplot2)
p=ggplot(data.frame(x,y),aes(x,y))
p+geom_point(size=3,alpha=0.8,
             colour=factor(result))


			 
			 
			 
			 
			 
###主成分分析
#install.packages('psych')
library(psych)
attach(data)
mat<-as.matrix(data)
rc<-principal(mat,nfactors=4,rotate=T,score=TRUE)
head(rc$scores)
mat1<-scale(mat)


###Fisher Test
mat<-as.matrix(data)
#fisher1<-fisher.test(mat,alternative = "less")




d<-dist(scale(data))
h<-hclust(d)
# 绘图
plclust(h)
# 31 个样本分为5 个大类
r<-rect.hclust(h,5)