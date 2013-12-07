#####################################
###1、画出常见的平稳线性序列的谱密度函数，
###如AR(1),AR(2),MA(1),MA(2),ARMA(1,1)，观察其特点。
#谱密度函数

#install.packages('TSA')
library(TSA)

###for AR(1)
phi=0.9 #Reset value of phi for other AR(1) models
ARMAspec(model=list(ar=phi))
###for AR(2)
phi1=1.5;phi2=-0.75 #Reset values of phi1 & phi2 for other AR(2) models.
ARMAspec(model=list(ar=c(phi1,phi2))) #AR(1)模型的谱密度函数

###for ARMA(1,1)
phi=0.5;theta=0.8
ARMAspec(model=list(ar=phi,ma=-theta))

###for MA(1)
theta=0.8
ARMAspec(model=list(ma=-theta))
###for MA(2)
theta1=1;theta2=-0.6
ARMAspec(model=list(ma=-c(theta1,theta2)))
##############################################3
###2、对于上一问题中每一个生成的序列，画出对应的周期图。
###同时讨论周期图与谱密度函数的相似程度与序列长度的关系。

#画出周期图
#arima.sim(n=500, list=c())
###for AR(1)
phi=0.9 #Reset value of phi for other AR(1) models
ar1_0.9<-arima.sim(n=500,list(ar=phi))
periodogram(ar1_0.9)
ARMAspec(model=list(ar=phi))

###AR(2)
phi1=1.5;phi2=-0.75
ar2<-arima.sim(n=500,list(ar=c(phi1,phi2)))
periodogram(ar2)
ARMAspec(model=list(ar=c(phi1,phi2))) #AR(1)模型的谱密度函数

###for ARMA(1,1)
phi=0.5;theta=0.8
arma11<-arima.sim(n=500,list(ar=phi,ma=-theta))
periodogram(arma11)
ARMAspec(model=list(ar=phi,ma=-theta))

###for MA(1)
theta=0.8
ma1<-arima.sim(n=500,list(ma=-theta))
periodogram(ma1)
ARMAspec(model=list(ma=-theta))

###for MA(2)
theta1=1;theta2=-0.6
ma2<-arima.sim(n=500,list(ma=-c(theta1,theta2)))
periodogram(ma2)
ARMAspec(model=list(ma=-c(theta1,theta2)))

#######################################################
###3、对于AR(1)和MA(1)，讨论不同参数取值下谱密度函数的形状差别。
###特别的，对于AR(1)模型，观察参数在平稳域内外时谱密度函数形状的异同。

###AR(1)无法画出平稳域外的图像


#######################################################
###4、对于实际数据，进行潜周期识别。
###（1）指定频率识别，周期T=0.5年，1年，2年。
###（2）未知多谐波识别。


#潜周期识别：
