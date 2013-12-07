y=X=data.frame(x1=c(4,1,3,3,7,4,6,5,3,6,4,4,5,7,5,10,7,4,9,5,8,6,7,8),x2=c(3,3,3,7,4,1,5,6,7,2,6,4,8,8,6,5,6,10,7,4,5,6,4,8),g=c(rep(1,10),rep(2,14)) )


#绘图
plot(x2~x1,col=c("red","blue")[g],data=y) 
#汇出新数据点（待归类的数据点）
points(5,6,pch=8,cex=3) 
#两个向量的欧式距离
dist.euclidean <- function(x,y){
res <- sqrt(sum((x-y)^2))
res}
#计算训练样本与新样本的距离
s=data.frame(x1=y$x1,x2=y$x2) 
apply(s,1,dist.euclidean,y=c(6,5)) 
#排序并查看类别
d <- cbind(apply(s,1,dist.euclidean,y=c(6,5))^2, X$g) 
o<-order(d[,1]) 
d1 <- d[o,] 


#假设k=8，使用8个最近邻点来确定新样本的分类，按照距离从小到大排序，取前8个最小值，查看这8个训练样本的分类，其中1类有3个，2类有5个，因此判断新样#本属于类2

#用R里class包中的knn函数实现如下：

#install.packages('class')
library(class)
knn(train=s,test=c(6,5),cl=y$g,k=2) #k=2时
knn(train=s,test=c(6,5),cl=y$g,k=8) #k=8时
knn(train=s,test=c(6,5),cl=y$g,k=1) #k=1时



library(class)
vknn = function(v,data,cl,k){
# 分割原始数据
grps = cut(1:nrow(data),v,labels=FALSE)[sample(1:nrow(data))]
# 对每份数据分别运行KNN函数
pred = lapply(1:v,function(i,data,cl,k){
  omit = which(grps == i)
  pcl = knn(data[-omit,],data[omit,],cl[-omit],k=k)
  },data,cl,k)
# 整合预测结果
wh = unlist(pred)
table(wh,cl[order(grps)])
pcol <- as.character(as.numeric(wh))
pairs(iris[1:4], pch = pcol, col = c("green3", "red")
	[(wh != cl[order(grps)])+1])

}
iris.vknn<-vknn(5,iris[1:4],iris$Species,5)


#当然，将上面的函数略加修改，即可用到其它的分类器上面。此外，class包中也有knn.cv函数，可以完成同样的工作。对于KNN算法，R语言中还有一个kknn包值得关注，它对于基本的knn函数有很大程度的扩展。它不只可以处理分类问题，还可以处理回归问题。另外它还可以结合核函数，利用距离进行加权计算。最后要说的是，使用KNN算法之前要先将数据标准化处理。









#使用R中kknn包中的kknn函数实现如下：
#install.packages('kknn')
library(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, 
	prob = rep(1/m, m))  #产生随机抽样组——3分之1作为测试组
iris.learn <- iris[-val,] #产生训练组
iris.valid <- iris[val,]  #产生测试组
iris.kknn <- kknn(Species~., iris.learn, iris.valid, distance = 1,
	kernel = "triangular")
summary(iris.kknn)
fit <- fitted(iris.kknn) #进行分类预测
table(iris.valid$Species, fit) #对预测结果进行验证
pcol <- as.character(as.numeric(iris.valid$Species))
pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")
	[(iris.valid$Species != fit)+1])


