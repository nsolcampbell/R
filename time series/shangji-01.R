#一、白噪声检验
#    实验讨论白噪声检验的不同方法的效果。对于简单的AR序列和MA序列，讨论被误判为白噪声的可能性大小。

#二、AR序列和MA序列的关系
#    对于简单的AR(p)序列，例如p=3，分别用AR(p-1),AR(p),AR(p+1)拟合，观察拟合的结果。
#    对于简单的MA(q)序列，例如q=2，分别用MA(q-1),MA(q),MA(q+1)拟合，观察拟合的结果。
#    AR序列能用MA序列近似估计吗？如果能，找出一个近似的q*=f(p)的关系。
#    MA序列能用AR序列近似估计吗？如果能，找出一个近似的p*=g(q)的关系。
#    可以讨论一些有代表性的序列，而不需要全面严谨的论证。

#三、以提供的EXCEL里的数据为例，讨论以下问题：

#1、如何判断序列是否为平稳序列？
#2、如果假定序列近似为平稳序列，建立合适的ARMA（或ARIMA）模型。
#3、如果假定序列从某时刻起，均值发生了明显的改变，如何找出这个“拐点”？（参考“门限模型”）



setwd('D:\\dropbox\\Dropbox\\手头工作')
data<-read.csv('1.csv')
names(data)
names(data)<-c('年','月','日','jc','hh','mjh')
attach(data)

##第一问
#白噪声检验
jc<-jc[!jc==-1]
hh<-hh[!hh==-1]
mjh<-mjh[!mjh==-1]
pacf(jc)
acf(jc)

pacf(hh);acf(hh)
pacf(mjh);acf(mjh)

jcbox<-Box.test(jc) #白噪声检验——Box-Pierce
jcLjung<-Box.test(jc, type="Ljung-Box")
hhbox<-Box.test(hh)
hhLjung<-Box.test(hh, type="Ljung-Box")
mjbox<-Box.test(mjh)
mjLjung<-Box.test(mjh, type="Ljung-Box")
#图示
#plot.ts(jc,main='泾川白噪声图')
#plot.ts(hh,main='红河白噪声图')
#plot.ts(mjh,main='毛家河白噪声图')

my.plot.ts <- function (x, main="") { 
    op <- par(mar=c(2,2,4,2)+.1)
    layout( matrix(c(1,2),nr=1,nc=2), widths=c(3,1) )
    plot(x, xlab="", ylab="")
    abline(h=0, lty=3)
    title(main=main)
    hist(x, col="light blue", main="", ylab="", xlab="")
    par(op)
    }
#n <- 100
#x <- ts(rnorm(n))
jcc<-ts(jc)
my.plot.ts(jcc, "泾川时间序列图")

#泾川
op <- par(mfrow=c(2,1), mar=c(5,4,2,2)+.1)
plot(ts(jc))
acf(jc, main = "")
par(op)
#红河
op <- par(mfrow=c(2,1), mar=c(5,4,2,2)+.1)
plot(ts(hh))
acf(hh, main = "")
par(op)
#毛家河
op <- par(mfrow=c(2,1), mar=c(5,4,2,2)+.1)
plot(ts(mjh))
acf(mjh, main = "")
par(op)


op <- par(mfrow=c(2,1))
plot.box.ljung <- function (jc,k = 15,main = "p-value of the Ljung-Box test",ylab = "p-value") {
    p <- rep(NA, k)
    for (i in 1:k) { 
        p[i] <- Box.test(jc, i,
        type = "Ljung-Box")$p.value
    }
    plot(p,type = "h",ylim = c(0,1),lwd = 3,main = main,ylab = ylab)
    abline(h = c(0,.05),lty = 3)
    }
plot.box.ljung(jc, main="泾川——Box.Ljung方法检验结果")
par(op)






##1.2
#产生AR模型
w<-rnorm(500)
x<-filter(w,c(0.2,0.3,0.5),'recursive')
plot.ts(x,main='产生AR模型图')
Box.test(x)
kn<-function(kn){
    x<-matrix(0,nrow=400,ncol=kn);p<-0
	for (k in 1:kn){
	w<-rnorm(400)
	x[,k]<-filter(w,c(0.2,0.3,0.5),'recursive')
	Box.test(x[,k])
	p[k]<-Box.test(x[,k])$p.value
	}
    p
	}
p<-kn(400)
	
n <- 200
x <- rnorm(n)
op <- par(mfrow=c(2,1))
y <- filter(x,.8,method="recursive")
plot(y, main="AR(1)", ylab="")
acf(y,main = paste("p =",signif( dwtest( y ~ 1 ) $ p.value, 3 )))
par(op)

#产生MA模型
y.ma<-function(a1,a2,a3=0,a4=0,num=200,pic=TRUE){#MA滑动平均时间序列的模拟（也可以使用filter函数）
    e<-rnorm(num,0,1)#模拟白噪声,均值=0
    result<-0
    result[1]<-e[1]
    result[2]<-e[2]-a1*e[1]
    result[3]<-e[3]-a1*e[2]-a2*e[1]
    result[4]<-e[4]-a1*e[3]-a2*e[2]-a3*e[1]
    for(t in 5:num){ result[t]<-e[t]-a1*e[t-1]-a2*e[t-2]-a3*e[t-3]-a4*e[t-4] }#构造一个ma型时间序列
    if(pic==TRUE){#画图形
        dev.new()
        ts.plot(result,main=paste("y.ma[t]=e[t]-",a1,"*e[t-1]-",a2,"*e[t-2]-",a3,"*e[t-3]-",a4,"*e[t-4]的时间序列散点图"))
        dev.new()
        lag.plot(result, 9, do.lines=FALSE)
        dev.new()
        par(mfrow=c(2,1))
        acf(result, 30,main=paste("y.ma自相关图,y.ma[t]=e[t]-",a1,"*e[t-1]-",a2,"*e[t-2]-",a3,"*e[t-3]-",a4,"*e[t-4]"))
        pacf(result, 30,main=paste("y.ma偏自相关图,y.ma[t]=e[t]-",a1,"*e[t-1]-",a2,"*e[t-2]-",a3,"*e[t-3]-",a4,"*e[t-4]"))
    }
    result
}
y.ma<-y.ma(0.92,0.65)
Box.test(y.ma)









###二、AR序列和MA序列的关系
#    对于简单的AR(p)序列，例如p=3，分别用AR(p-1),AR(p),AR(p+1)拟合，观察拟合的结果。
#    对于简单的MA(q)序列，例如q=2，分别用MA(q-1),MA(q),MA(q+1)拟合，观察拟合的结果。
#    AR序列能用MA序列近似估计吗？如果能，找出一个近似的q*=f(p)的关系。
#    MA序列能用AR序列近似估计吗？如果能，找出一个近似的p*=g(q)的关系。
#    可以讨论一些有代表性的序列，而不需要全面严谨的论证。

w<-rnorm(500)
ar.x<-filter(w,c(0.2,0.3,0.5),'recursive')
plot.ts(x,main='产生AR模型图')
ar2<-ar(ar.x,aic=F,order.max=2,method='ols')


#Call:
#ar(x = ar.x, aic = F, order.max = 2, method = "ols")

#Coefficients:
#     1       2  
#0.4029  0.5244  

#Intercept: -0.01261 (0.04623) 

#Order selected 2  sigma^2 estimated as  1.064

ar3<-ar(ar.x,aic=F,order.max=3,method='ols')

#Call:
#ar(x = ar.x, aic = F, order.max = 3, method = "ols")

#Coefficients:
#     1       2       3  
#0.1779  0.3506  0.4288  

#Intercept: -0.01657 (0.04183) 

#Order selected 3  sigma^2 estimated as  0.8696

ar4<-ar(ar.x,aic=F,order.max=4,method='ols')

#Call:
#ar(x = ar.x, aic = F, order.max = 4, method = "ols")

#Coefficients:
#     1       2       3       4  
#0.1547  0.3306  0.4189  0.0548  

#Intercept: -0.01838 (0.04185) 

#Order selected 4  sigma^2 estimated as  0.8683


ma.x<-filter(w,c(0.2,0.3,0.5),'convolution')

ma2<-arima(ma.x,order=c(0,0,2),method='ML')

#Call:
#arima(x = ma.x, order = c(0, 0, 2), method = "ML")

#Coefficients:
#         ma1     ma2  intercept
#      0.5969  0.4498    -0.0157
#s.e.  0.0420  0.0409     0.0429

#sigma^2 estimated as 0.2198:  log likelihood = -329.76,  aic = 665.51

ma3<-arima(ma.x,order=c(0,0,3),method='ML')

Call:
arima(x = ma.x, order = c(0, 0, 3), method = "ML")

#Coefficients:
#         ma1     ma2      ma3  intercept
#      0.5530  0.3868  -0.1024    -0.0158
#s.e.  0.0455  0.0496   0.0492     0.0384

#sigma^2 estimated as 0.218:  log likelihood = -327.67,  aic = 663.34

ma4<-arima(ma.x,order=c(0,0,4),method='ML')

#Call:
#arima(x = ma.x, order = c(0, 0, 4), method = "ML")

#Coefficients:
#         ma1     ma2      ma3      ma4  intercept
#      0.5546  0.3707  -0.1181  -0.0336    -0.0158
#s.e.  0.0447  0.0518   0.0525   0.0434     0.0371

#sigma^2 estimated as 0.2177:  log likelihood = -327.37,  aic = 664.74





#三、以提供的EXCEL里的数据为例，讨论以下问题：

#1、如何判断序列是否为平稳序列？
#2、如果假定序列近似为平稳序列，建立合适的ARMA（或ARIMA）模型。
#3、如果假定序列从某时刻起，均值发生了明显的改变，如何找出这个“拐点”？（参考“门限模型”）



#3.1检验方法——一:图形检验二:单位根检验,ADF检验,PP检验.
#平稳时间序列是ARMA模型的建模前提，对时间序列进行平稳性的判断是预测的首要步骤。一般而言，所谓的平稳时间序列是指宽平稳过程，
#即是一阶距与时间无关，二阶距只与时间间隔有关。有几种判断方法：
#1）数据图直接检验法。画出X(t)的图像，当x(t)围绕某一水平线上下波动而无明显上升、下降或周期趋势时，则认为x(t）是平稳的。（简单直接，我喜欢！）
#2）自相关、偏相关函数检验法。一个零均值平稳序列的自相关函数和偏自相关函数要么是截尾的，要么是拖尾的。
#因此，如果一个序列零均值化以后的自相关函数或偏自相关函数既不截尾，又不拖尾则可以断定该序列时非平稳的。（方法有点晕，还需要再查查资料？？）
#3）特征根检验法。先拟合序列的适应模型，然后求由适应模型的参数组成的特征方程的特征根，若所有的特征根都满足平稳性条件，
#abs(namta)<1则可认为该序列时平稳的，否则该序列是非平稳的。其中namta为特征根。
#4）Rk检验。根据Green函数Gj与自协方差函数Rk之间的关系，当{xt}是平稳时间序列时，Gj的极限趋于0，
#表示稳定系统的单位脉冲响应最终将衰减至0。

#3.1.1图形检验，画出X(t)的图像，当x(t)围绕某一水平线上下波动而无明显上升、下降或周期趋势时，则认为x(t）是平稳的。
plot(jc)
#3.1.2自相关偏相关检验法，一个零均值平稳序列的自相关函数和偏自相关函数要么是截尾的，要么是拖尾的。因此，如果一个序列零均值化以后的自相关函数或偏自相关函数既不截尾，又不拖尾则可以断定该序列时非平稳的。
pacf(jc)
acf(jc)
#3.1.2单位根检验，先拟合序列的适应模型，然后求由适应模型的参数组成的特征方程的特征根，若所有的特征根都满足平稳性条件，abs(namta)<1则可认为该序列时平稳的，否则该序列是非平稳的。其中namta为特征根。
library(urca)
ur.df(jc)
jcpingwen<-ur.df(jc)
summary(jcpingwen)


#3.2如果假定序列近似为平稳序列，建立合适的ARMA（或ARIMA）模型。
#   如果是ARMA(p,q) 类型的，看eacf图，看x三角形的顶端
library(TSA)
plot.cf<-function(data){
    dev.new()
    ts.plot(data,main="时间序列散点图")
    dev.new()
    lag.plot(data, 9, do.lines=FALSE)
    dev.new()
    par(mfrow=c(2,1))
    acf(data, 30,main="自相关图")
    pacf(data, 30,main="偏自相关图")
    eacf(data)
}
plot.cf(jc)
#因此p为1，q为2

p=1;d=0;q=2
jc.ts<-ts(jc)#把向量化为时间序列,freq=1,star=1
sol<-arima(jc.ts,order=c(p,d,q))#产生模型。arima只处理时间序列，不处理向量等
dev.new()
plot(sol,n.ahead=30)#预测30个点


#3、如果假定序列从某时刻起，均值发生了明显的改变，如何找出这个“拐点”？（参考“门限模型”）
#趋势项分解
model<-stl(jc, s.windows=period)


