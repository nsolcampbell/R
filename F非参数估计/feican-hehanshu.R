#案例1
plot(density(rep(0, 1000))) #可以看到它得到了正态分布的曲线，但实际上从数据上判断，它更有可能是一个退化的单点分布。

#案例2
#但是核密度可以解决许多模拟中存在的异方差问题。比如说我们要估计一下下面的一组数据：
set.seed(10)  
dat<-c(rgamma(300,shape=2,scale=2),rgamma(100,shape=10,scale=2)) 
#可以看出它是由300个服从gamma（2,2）与100个gamma（10,2）的随机数构成的，
#他用参数统计的办法是没有办法得到一个好的估计的。那么我们尝试使用核密度估计：
plot(density(dat),ylim=c(0,0.2)) 

#将利用正态核密度与标准密度函数作对比

dfn<-function(x,a,alpha1,alpha2,theta){  
a*dgamma(x,shape=alpha1,scale=theta)+(1-a)*dgamma(x,shape=alpha2,scale=theta)}  
pfn<-function(x,a,alpha1,alpha2,theta){  
a*pgamma(x,shape=alpha1,scale=theta)+(1-a)*pgamma(x,shape=alpha2,scale=theta)}  
curve(dfn(x,0.75,2,10,2),add=T,col="red")  

#会得到一幅图像
#（红色的曲线为真实密度曲线）
#可以看出核密度与真实密度相比，得到大致的估计是不成问题的。至少趋势是得到了的。如果换用gamma分布的核效果无疑会更好，
#但是遗憾的是r中并没有提供那么多的核供我们挑选（其实我们知道核的选择远没有窗宽的选择来得重要），所以也无需介怀。
#R中提供的核：kernel = c("gaussian", "epanechnikov", "rectangular",                   "triangular", "biweight","cosine", "optcosine")。




