setwd('d:\\dropbox\\Dropbox\\Apps\\R\\time series\\时间序列上机\\时间序列-倪')
#####################################
#install.packages('TSA')
library(TSA)
##################################################
#######画出常见的平稳线性序列的谱密度函数，如AR(1),AR(2),MA(1),MA(2),ARMA(1,1)，观察其特点。
#######周期图及谱密度
###产生序列
#arima.sim(n=500, list=c())

###for AR(1)
#谱密度函数
phi=0.9 #Reset value of phi for other AR(1) models
ar1_0.9<-arima.sim(n=500,list(ar=phi))
periodogram(ar1_0.9)
op <- par(mfrow=c(3,3))
ARMAspec(model=list(ar=0.9))
ARMAspec(model=list(ar=-0.8))
ARMAspec(model=list(ar=0.7))
ARMAspec(model=list(ar=-0.6))
ARMAspec(model=list(ar=0.5))
ARMAspec(model=list(ar=-0.4))
ARMAspec(model=list(ar=0.3))
ARMAspec(model=list(ar=-0.2))
ARMAspec(model=list(ar=0.1))
#周期图
ar1.1<-arima.sim(n=500,list(ma=0.9))
ar1.2<-arima.sim(n=500,list(ma=-0.8))
ar1.3<-arima.sim(n=500,list(ma=0.7))
ar1.4<-arima.sim(n=500,list(ma=-0.6))
ar1.5<-arima.sim(n=500,list(ma=0.5))
ar1.6<-arima.sim(n=500,list(ma=-0.4))
ar1.7<-arima.sim(n=500,list(ma=0.3))
ar1.8<-arima.sim(n=500,list(ma=-0.2))
ar1.9<-arima.sim(n=500,list(ma=0.1))
op <- par(mfrow=c(3,3))
periodogram(ar1.1);periodogram(ar1.2);periodogram(ar1.3)
periodogram(ar1.4);periodogram(ar1.5);periodogram(ar1.6)
periodogram(ar1.7);periodogram(ar1.8);periodogram(ar1.9)
###AR(2)
#谱密度函数
phi1=1.5;phi2=-0.75
ar2<-arima.sim(n=500,list(ar=c(phi1,phi2)))
periodogram(ar2)
op <- par(mfrow=c(3,3))
ARMAspec(model=list(ar=c(1.5,-0.75))) #AR(2)模型的谱密度函数
ARMAspec(model=list(ar=c(1,-0.75))) #AR(2)模型的谱密度函数
ARMAspec(model=list(ar=c(0.9,-0.75))) #AR(2)模型的谱密度函数
ARMAspec(model=list(ar=c(0.5,-0.25))) #AR(2)模型的谱密度函数
ARMAspec(model=list(ar=c(0.1,0))) #AR(2)模型的谱密度函数
ARMAspec(model=list(ar=c(-0.2,0.1))) #AR(2)模型的谱密度函数
ARMAspec(model=list(ar=c(-0.6,0.3))) #AR(2)模型的谱密度函数
ARMAspec(model=list(ar=c(-0.6,-0.3))) #AR(2)模型的谱密度函数
ARMAspec(model=list(ar=c(-0.8,-0.9))) #AR(2)模型的谱密度函数
#周期图
ar2.1<-arima.sim(n=500,list(ma=-c(1.5,-0.75)))
ar2.2<-arima.sim(n=500,list(ma=-c(1,-0.75)))
ar2.3<-arima.sim(n=500,list(ma=-c(0.9,-0.75)))
ar2.4<-arima.sim(n=500,list(ma=-c(0.5,-0.25)))
ar2.5<-arima.sim(n=500,list(ma=-c(0.1,0)))
ar2.6<-arima.sim(n=500,list(ma=-c(-0.2,0.1)))
ar2.7<-arima.sim(n=500,list(ma=-c(-0.6,0.3)))
ar2.8<-arima.sim(n=500,list(ma=-c(-0.6,-0.3)))
ar2.9<-arima.sim(n=500,list(ma=-c(-0.8,-0.9)))
op <- par(mfrow=c(3,3))
periodogram(ar2.1);periodogram(ar2.2);periodogram(ar2.3)
periodogram(ar2.4);periodogram(ar2.5);periodogram(ar2.6)
periodogram(ar2.7);periodogram(ar2.8);periodogram(ar2.9)
###for MA(1)
#谱密度函数
op <- par(mfrow=c(3,3))
ARMAspec(model=list(ma=-0.8))
ARMAspec(model=list(ma=-0.7))
ARMAspec(model=list(ma=-0.6))
ARMAspec(model=list(ma=-0.5))
ARMAspec(model=list(ma=-0.4))
ARMAspec(model=list(ma=-0.3))
ARMAspec(model=list(ma=-0.2))
ARMAspec(model=list(ma=-0.1))
ARMAspec(model=list(ma=0))
#周期图
ma1.1<-arima.sim(n=500,list(ma=-0.8))
ma1.2<-arima.sim(n=500,list(ma=-0.7))
ma1.3<-arima.sim(n=500,list(ma=-0.6))
ma1.4<-arima.sim(n=500,list(ma=-0.5))
ma1.5<-arima.sim(n=500,list(ma=-0.4))
ma1.6<-arima.sim(n=500,list(ma=-0.3))
ma1.7<-arima.sim(n=500,list(ma=-0.2))
ma1.8<-arima.sim(n=500,list(ma=-0.1))
ma1.9<-arima.sim(n=500,list(ma=-0))
op <- par(mfrow=c(3,3))
periodogram(ma1.1);periodogram(ma1.2);periodogram(ma1.3)
periodogram(ma1.4);periodogram(ma1.5);periodogram(ma1.6)
periodogram(ma1.7);periodogram(ma1.8);periodogram(ma1.9)
###for MA(2)
theta1=1;theta2=-0.6
ma2<-arima.sim(n=500,list(ma=-c(theta1,theta2)))
periodogram(ma2)
op <- par(mfrow=c(3,3))
ARMAspec(model=list(ma=-c(1,-1)))
ARMAspec(model=list(ma=-c(0.8,-0.75)))
ARMAspec(model=list(ma=-c(0.5,-0.6)))
ARMAspec(model=list(ma=-c(0,0)))
ARMAspec(model=list(ma=-c(-0.2,0.3)))
ARMAspec(model=list(ma=-c(-0.7,0.8)))
ARMAspec(model=list(ma=-c(-1,1)))
ARMAspec(model=list(ma=-c(-1,-1)))
ARMAspec(model=list(ma=-c(1,1)))
#周期图
ma2.1<-arima.sim(n=500,list(ma=-c(1,-1)))
ma2.2<-arima.sim(n=500,list(ma=-c(0.8,-0.75)))
ma2.3<-arima.sim(n=500,list(ma=-c(0.5,-0.6)))
ma2.4<-arima.sim(n=500,list(ma=-c(0,0)))
ma2.5<-arima.sim(n=500,list(ma=-c(-0.2,0.3)))
ma2.6<-arima.sim(n=500,list(ma=-c(-0.7,0.8)))
ma2.7<-arima.sim(n=500,list(ma=-c(-1,1)))
ma2.8<-arima.sim(n=500,list(ma=-c(-1,-1)))
ma2.9<-arima.sim(n=500,list(ma=-c(1,1)))
op <- par(mfrow=c(3,3))
periodogram(ma2.1);periodogram(ma2.2);periodogram(ma2.3)
periodogram(ma2.4);periodogram(ma2.5);periodogram(ma2.6)
periodogram(ma2.7);periodogram(ma2.8);periodogram(ma2.9)
###for arma(1,1)
#谱密度函数
op <- par(mfrow=c(3,3))
ARMAspec(model=list(ar=0.5,ma=-0.8))
ARMAspec(model=list(ar=0.3,ma=-0.4))
ARMAspec(model=list(ar=0.3,ma=-0.1))
ARMAspec(model=list(ar=0,ma=0))
ARMAspec(model=list(ar=0.1,ma=-0.4))
ARMAspec(model=list(ar=-0.3,ma=0.4))
ARMAspec(model=list(ar=-0.5,ma=0.8))
ARMAspec(model=list(ar=-0.5,ma=-0.8))
ARMAspec(model=list(ar=0.5,ma=0.8))
#周期图
arma.1<-arima.sim(n=500,list(ar=0.5,ma=-0.8))
arma.2<-arima.sim(n=500,list(ar=0.3,ma=-0.4))
arma.3<-arima.sim(n=500,list(ar=0.3,ma=-0.1))
arma.4<-arima.sim(n=500,list(ar=0.000000000000001,ma=0.00000000000000001))
arma.5<-arima.sim(n=500,list(ar=0.1,ma=-0.4))
arma.6<-arima.sim(n=500,list(ar=-0.3,ma=0.4))
arma.7<-arima.sim(n=500,list(ar=-0.5,ma=0.8))
arma.8<-arima.sim(n=500,list(ar=-0.5,ma=-0.8))
arma.9<-arima.sim(n=500,list(ar=0.5,ma=0.8))
op <- par(mfrow=c(3,3))
periodogram(arma.1);periodogram(arma.2);periodogram(arma.3)
periodogram(arma.4);periodogram(arma.5);periodogram(arma.6)
periodogram(arma.7);periodogram(arma.8);periodogram(arma.9)





############动态展示
library(animation)
#AR1模型
## set some options first
oopt = ani.options(interval = 0.2, nmax = 20)
## use a loop to create images one by one
for (i in 1:ani.options("nmax")) {
	ARMAspec(model=list(ar=(i-10)/10))
    ani.pause()  ## pause for a while ('interval')
}
## restore the options
ani.options(oopt)

#AR2模型
## set some options first
oopt = ani.options(interval = 0.2, nmax = 20)
## use a loop to create images one by one
for (i in 1:ani.options("nmax")) {
    for (j in 1:ani.options("nmax")) {
	ARMAspec(model=list(ar=c((i-10)/10,(j-10)/10))) #AR(2)模型的谱密度函数
    ani.pause()  ## pause for a while ('interval')
}
}
## restore the options
ani.options(oopt)

#MA1模型
## set some options first
oopt = ani.options(interval = 0.2, nmax = 20)
## use a loop to create images one by one
for (i in 1:ani.options("nmax")) {
	ARMAspec(model=list(ma=(i-20)/20))
    ani.pause()  ## pause for a while ('interval')
}
## restore the options
ani.options(oopt)

#MA2模型
## set some options first
oopt = ani.options(interval = 0.2, nmax = 20)
## use a loop to create images one by one
for (i in 1:ani.options("nmax")) {
for (j in 1:ani.options("nmax")) {
	ARMAspec(model=list(ma=-c(-(i-10)/10,-(j-10)/10)))
    ani.pause()  ## pause for a while ('interval')
}
}
## restore the options
ani.options(oopt)

#ARMA模型
## set some options first
oopt = ani.options(interval = 0.2, nmax = 20)
## use a loop to create images one by one
for (i in 1:ani.options("nmax")) {
    for (j in 0:10-i) {
	ARMAspec(model=list(ar=(i-10)/10,ma=(j-10)/10))
    ani.pause()  ## pause for a while ('interval')
}
}
## restore the options
ani.options(oopt)





##################################################
#######Q4
setwd('d:\\dropbox\\Dropbox\\Apps\\R\\time series\\时间序列上机\\时间序列-倪')
rivers<-read.csv('rivers.csv')
names(rivers)<-c('year','month','day','jc','hh','mjh')
attach(rivers)



#平稳性检验
#install.packages("tseries")
library(tseries)
library(TSA)
library(stats)
adf.test(jc)
adf.test(hh)
adf.test(mjh)
######JC
#周期图
periodogram(jc)
#FFT变换
spec.pgram(jc)
barplot(Mod(jc.ft)) #mod相当于abs
t=1:4383
plot(t,jc,type='o')
period.jc<-periodogram(jc);abline(h=0)
period.jc$freq[which.max(period.jc$spec)]
period.jc$spec[which.max(period.jc$freq)]
reg.jc.1<-lm(jc~cos(4*pi*t)+sin(4*pi*t))
summary(reg.jc.1)
reg.jc.2<-lm(jc~cos(2*pi*t)+sin(2*pi*t))
summary(reg.jc.2)
reg.jc.3<-lm(jc~cos(pi*t)+sin(pi*t))
summary(reg.jc.3)
######hh
#周期图
periodogram(hh)
#FFT变换
spec.pgram(hh) #spec.pgram calculates the periodogram using a fast Fourier transform, and optionally smooths the result with a series of modified Daniell smoothers (moving averages giving half weight to the end values).
barplot(Mod(hh.ft)) #mod相当于abs
t=1:4383
plot(t,hh,type='o')
period.hh<-periodogram(hh);abline(h=0)
per1<-period.hh$spec[-c(1:100)] #去掉前边不好的数据
per2<-period.hh$freq[-c(1:100)] #去掉前边不好的数据
plot(per2,per1,type='o')
period.hh$freq[which.max(period.hh$spec)]
period.hh$spec[which.max(period.hh$freq)]
reg.hh.1<-lm(hh~cos(4*pi*t)+sin(4*pi*t))
summary(reg.hh.1)
reg.hh.2<-lm(hh~cos(2*pi*t)+sin(2*pi*t))
summary(reg.hh.2)
reg.hh.3<-lm(hh~cos(pi*t)+sin(pi*t))
summary(reg.hh.3)
######mjh
#周期图
periodogram(mjh)
#FFT变换
spec.pgram(mjh)
barplot(Mod(mjh.ft)) #mod相当于abs
t=1:4383
plot(t,mjh,type='o')
period.mjh<-periodogram(mjh);abline(h=0)
period.mjh$freq[which.max(period.mjh$spec)]
period.mjh$spec[which.max(period.mjh$freq)]
reg.mjh.1<-lm(mjh~cos(4*pi*t)+sin(4*pi*t))
summary(reg.mjh.1)
reg.mjh.2<-lm(mjh~cos(2*pi*t)+sin(2*pi*t))
summary(reg.mjh.2)
reg.mjh.3<-lm(mjh~cos(pi*t)+sin(pi*t))
summary(reg.mjh.3)




#去除噪声
library(RTisean)
project(hh)
x <- 1:500
y <- cos(x/100)^2 - cos(x/200)+ rnorm(500)/10
filteredy <- project(y,m=7,q=2,k=10,r=1)
plot(x, y, t="l", xlab="Time", ylab="Time series", main="Projective nonlinear noise reduction")
lines(x, filteredy[,1], col=2,lwd=1.5)
legend(350,0, c("Noisy data","Filtered data"),fill=c(1,2), bty="n")


A<-matrix(c(1),nrow=1)
G <-matrix(c(1),nrow=1)
C <-matrix(c(1),nrow=1)
Q<-matrix(c(2),nrow=1)
R <-matrix(c(1),nrow=1)
X0 = 0
P0 = 10
N = 500
Z = rnorm(N, mean=25, sd=1)
X = kalman(A,G,C,Q,R,X0,P0,hh)
par(mfrow=c(1,2))
plot(c(1:N), Z)
lines(c(1:N), X)
title("Estimate(line) and Measurement Values")
plot(c(1:N), (Z-25))
lines(c(1:N), (X-25))
title("Error Values")

