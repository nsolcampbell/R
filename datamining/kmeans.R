#聚类分析

#聚类分析有两种主要计算方法，分别是凝聚层次聚类（Agglomerative hierarchical method）和K均值聚类（K-Means）。

#一、层次聚类

#下面我们用iris数据集来进行聚类分析，在R语言中所用到的函数为hclust。
#首先提取iris数据中的4个数值变量，然后计算其欧氏距离矩阵。然后将矩阵绘制热图，从图中可以看到颜色越深表示样本间距离越近，
#大致上可以区分出三到四个区块，其样本之间比较接近。
data=iris[,-5] #采用iris数据
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
             aes(colour=factor(result),
               shape=iris$Species))

			   
			   
#二、K均值聚类
#K均值聚类又称为动态聚类，它的计算方法较为简单，也不需要输入距离矩阵。
#首先要指定聚类的分类个数N，随机取N个样本作为初始类的中心，计算各样本与类中心的距离并进行归类，
#所有样本划分完成后重新计算类中心，重复这个过程直到类中心不再变化。

#在R中使用kmeans函数进行K均值聚类，centers参数用来设置分类个数，nstart参数用来设置取随机初始中心的次数，
#其默认值为1，但取较多的次数可以改善聚类效果。model2$cluster可以用来提取每个样本所属的类别。 

data=iris[,-5] #采用iris数据
dist.e=dist(data,method='euclidean')  #计算距离

model2=kmeans(data,centers=3,nstart=10)
result2=cutree(model2,k=3)


mds=cmdscale(dist.e,k=2,eig=T)
x = mds$points[,1]
y = mds$points[,2]
library(ggplot2)
p=ggplot(data.frame(x,y),aes(x,y))
p+geom_point(size=3,alpha=0.8,
             aes(colour=factor(model2$cluster),
               shape=iris$Species))

#使用K均值聚类时需要注意，只有在类的平均值被定义的情况下才能使用，还要求事先给出分类个数。
#一种方法是先用层次聚类以决定个数，再用K均值聚类加以改进。或者以轮廓系数来判断分类个数。
#改善聚类的方法还包括对原始数据进行变换，如对数据进行降维后再实施聚类。

#cluster扩展包中也有许多函数可用于聚类分析，如agnes函数可用于凝聚层次聚类，
#diana可用于划分层次聚类，pam可用于K均值聚类，fanny用于模糊聚类。




###二


#生成测试数据集
 x1<-rnorm(40,0,1)
 y1<-rnorm(40,0,1)
 x2<-rnorm(35,9,1)
 y2<-rnorm(35,3,1)
 x3<-rnorm(30,1,1)
 y3<-rnorm(30,-6,1)
 testpoint<-data.frame(x=c(x1,x2,x3),y=c(y1,y2,y3))
 clu=37

#画图
#x11()
plot(testpoint,cex=1.5)
center0<-testpoint[sample(nrow(testpoint),clu),]     #随机取中心
points(center0,col=rainbow(clu),pch=19,cex=1.5)    #标出中心

#迭代过程
repeat
{
 eps=0.0001                              #定义收敛精度
 result<-center0 
 center1<-center0
 d0<-matrix(0,nr=nrow(testpoint),nc=nrow(center0))

 neighbor<-NULL
 for(i in 1:nrow(testpoint))                          #新中心的确定
  {
   for(j in 1:nrow(center0))
     d0[i,j]<-sum((testpoint[i,]-center0[j,])^2)
   neighbor[i]<-which.min(d0[i,])
  }
 for(k in 1:nrow(center0))
  center1[k,]<-mean(testpoint[neighbor==k,])
  if (sum((center1-center0)^2)<eps) break;        #设置break条件
 points(center1,col=rainbow(clu),pch=19,cex=1.5)           #画新中心
 arrows(center0[,1],center0[,2],center1[,1],center1[,2],     #画迭代路径
        lty=2,col=rainbow(clu))
 center0=center1
}
for(i in 1:nrow(center1))                              #显示最后聚类结果
  points(testpoint[neighbor==i,],
         col=rainbow(clu)[i],pch=2,cex=1.5)

##六种的测试数据集：

 x1<-rnorm(40,0,1)
 y1<-rnorm(40,0,1)
 x2<-rnorm(35,9,1)
 y2<-rnorm(35,3,1)
 x3<-rnorm(30,1,1)
 y3<-rnorm(30,-6,1)
 x4<-rnorm(40,2,1)
 y4<-rnorm(40,7,1)
 x5<-rnorm(35,9,1)
 y5<-rnorm(35,9,1)
 x6<-rnorm(30,5,1)
 y6<-rnorm(30,-5,1) 
 testpoint<-data.frame(x=c(x1,x2,x3,x4,x5,x6),y=c(y1,y2,y3,y4,y5,y6))
 clu=7

 plot(testpoint,cex=1.5)
 center0<-testpoint[sample(nrow(testpoint),clu),]     #随机取中心
points(center0,col=rainbow(clu),pch=19,cex=1.5)    #标出中心

#迭代过程
repeat
{
 eps=0.0001                              #定义收敛精度
 result<-center0 
 center1<-center0
 d0<-matrix(0,nr=nrow(testpoint),nc=nrow(center0))

 neighbor<-NULL
 for(i in 1:nrow(testpoint))                          #新中心的确定
  {
   for(j in 1:nrow(center0))
     d0[i,j]<-sum((testpoint[i,]-center0[j,])^2)
   neighbor[i]<-which.min(d0[i,])
  }
 for(k in 1:nrow(center0))
  center1[k,]<-mean(testpoint[neighbor==k,])
  if (sum((center1-center0)^2)<eps) break;        #设置break条件
 points(center1,col=rainbow(clu),pch=19,cex=1.5)           #画新中心
 arrows(center0[,1],center0[,2],center1[,1],center1[,2],     #画迭代路径
        lty=2,col=rainbow(clu))
 center0=center1
}
for(i in 1:nrow(center1))                              #显示最后聚类结果
  points(testpoint[neighbor==i,],
         col=rainbow(clu)[i],pch=2,cex=1.5)
		 

###三、关于轮廓系数



newiris <- iris
newiris$Species <- NULL
kc <- kmeans(newiris, 3) #用k均值法分成了三类
plot(newiris[c("Sepal.Length", "Sepal.Width")], col = kc$cluster) #按照kmeans计算方法得到的分类结果(cluster)来分配颜色画图
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2) #在画出的图上面把点标出来


#在R语言中package fpc可以计算聚类后的一些评价指标，其中就包括了轮廓系数。

#首先加载软件包
library(fpc)
#利用著名的iris数据集
data=iris[,1:4]
#设簇个数在2到5之间取值
x<-c()
for (i in 2:5){
#K聚类结果存于result变量
result <- kmeans(data,i)
#求出聚类评价统计量
stats=cluster.stats(dist(data), result$cluster)
#将结果存入X
x[i]=stats$avg.silwidth
}
#得到 0.6810462 0.5528190 0.4104276 0.4912400
#说明在分成两类最好


#但是即便如此，Kmeans还是有缺陷，如下

# 生成数据
x1 <- seq(0,pi,length.out=100)
y1 <- sin(x1) + 0.1*rnorm(100)
x2 <- 1.5+ seq(0,pi,length.out=100)
y2 <- cos(x2) + 0.1*rnorm(100)
data <- data.frame(c(x1,x2),c(y1,y2))
names(data) <- c('x','y')
 
# 用K均值聚类
model1 <- kmeans(data,centers=2,nstart=10)
library(ggplot2)
p <- ggplot(data,aes(x,y))
p + geom_point(size=2.5,aes(colour=factor(model1$cluster)))+
               opts(legend.position='top')
 
#因此可以看到，K均值方法对于球状、环状数据的支持不好


x1 <- seq(0,pi,length.out=100)
y1 <- sin(x1) + 0.1*rnorm(100)
x2 <- 1.5+ seq(0,pi,length.out=100)
y2 <- cos(x2) + 0.1*rnorm(100)
data <- data.frame(c(x1,x2),c(y1,y2))
names(data) <- c('x','y')


# 用fpc包中的dbscan函数进行密度聚类
library(fpc);library(ggplot2)
model2 <- dbscan(data,eps=0.6,MinPts=4)
p + geom_point(size=2.5, aes(colour=factor(model2$cluster)))+
               opts(legend.position='top')