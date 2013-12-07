#set.seed(20)
#x<-runif(20)
#plot(density(x))
z<-as.matrix(read.csv("C:\\Users\\nixujun\\Desktop\\1.csv",header=F))
plot(density(z[,1]))


z<-as.matrix(read.csv("C:\\Users\\nixujun\\Desktop\\1.csv",header=F))
z1<-z[,1][!is.na(z[,1])]
plot(density(z1),main="",xlab="",ylab="")
title(xlab="生活状态感知指数",ylab="分数密度")

z2<-z[,2][!is.na(z[,2])]
plot(density(z2),main="",xlab="",ylab="")
title(xlab="生活环境感知指数",ylab="分数密度")

z3<-z[,3][!is.na(z[,3])]
plot(density(z3),main="",xlab="",ylab="")
title(xlab="居民素质感知指数",ylab="分数密度")

z4<-z[,4][!is.na(z[,4])]
plot(density(z4),main="",xlab="",ylab="")
title(xlab="公共服务感知指数",ylab="分数密度")


#density(z1)是什么东西？
#这也不行plot(z1,dnorm(z1),type="l",main="",xlab="",ylab="")


x=seq(-3,3,.05)
plot(x,dnorm(x),type="l")