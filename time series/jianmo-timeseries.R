data(sunspots)
mean(sunspots)#51.26596
var(sunspots)#1887.813
fivenum(sunspots)#0.00  15.70  42.00  74.95 253.80
plot(sunspots,type="b",xlab="i",ylab=expression(X[i]),col=2)
#趋势分析
decompose(sunspots)#$figure
 #[1] -2.1619020 -0.1085010 -1.2754348 -0.2400181  1.1422112  0.2472504
 #[7]  0.1120830  0.7560467  1.0681372  0.6220011 -0.5170302  0.3551564
#没有很强的季节性，季节性指数大多在0附近。
plot(decompose(sunspots))
#没有季节性，趋势性不明显,没有增长趋势，这也是符合自然常识的。
adf.test(sunspots)
#数据仍然有非常好的平稳性
par(mfrow=c(1,2))
acf(sunspots)
pacf(sunspots)#see picture 5
#这里可以看出acf没有明显的周期性，
sunspots1<-diff(sunspots,lag=1)#从acf来看，一阶差分是有必要的
adf.test(sunspots1)#stationary
par(mfrow=c(1,2))
acf(sunspots1)
pacf(sunspots1)
stl(sunspots1,"per")
plot(stl(sunspots1,"per"))
par(mfrow=c(2,1))
acf(sunspots1)
pacf(sunspots1)#得到了非常好的平稳性数据，那么前面做的平稳性检验错了吗？
#从acf来看，差分后的数据应该是一个ar（2）模型，给出模型估计
ar2<-arima(sunspots1,order=c(2,0,0))
ar2
#Coefficients:
#          ar1      ar2  intercept
#      -0.3629  -0.2064    -0.0096
#s.e.   0.0184   0.0184     0.1926
#sigma^2 estimated as 257.3:  log likelihood = -11823.37,  aic = 23654.74
#建模确定为arima（2,1,0）
sunspots.fit1<-arima(sunspots,order=c(2,1,0))
#arima(x = sunspots, order = c(2, 1, 0))
#Coefficients:
#          ar1      ar2
#      -0.3629  -0.2064
#s.e.   0.0184   0.0184
#sigma^2 estimated as 257.3:  log likelihood = -11823.37,  aic = 23652.74
library(forecast)
sunspots.forecast1 <- forecast(sunspots.fit1,12)
plot.forecast(sunspots.forecast1)
#前面的时间序列文章中我们曾确定使用ar（2）模型，这个模型好吗？
sunspots.fit<-arima(sunspots,order=c(2,0,0))
#arima(x = sunspots, order = c(2, 0, 0))
#Coefficients:
#         ar1     ar2  intercept
#      0.6704  0.2722    51.2642
#s.e.  0.0181  0.0181     5.2842
#sigma^2 estimated as 262.8:  log likelihood = -11858.25,  aic = 23724.5
#思考：从aic判定角度来看，差别并不是十分大，这是不是说明了通过平稳性检验未必见得是真正平稳的，可能一阶差分后效果更好？
library(forecast)
sunspots.forecast <- forecast(sunspots.fit,12)
plot.forecast(sunspots.forecast)
#部分运行结果以注释的形式给出，这段代码主要展示了时间序列模型识别，定阶，参数估计，模型判断的R语言示例

#用pandit-wu法处理
aico=resi<-matrix(0,nrow=6)
for (i in 1:6) {
a<-arima(sunspots,order=c(2*i,1,2*i-1))
aico[i]<-a$aic
resi[i]=sum(a$resi^2)
}
aico
# 首先对ARMA(2,1)和ARMA(4,3)作F检验：
F<-(resi[1,1]-resi[2,1])*2809/(4*resi[2,1])  #s=4,p+q=7 N1=2820-2-2 length(sunspots)
F<qf(0.95,4,2089)#FALSE
#这与课堂上的结论不符，ARMA（4,3）是有显著改进的
F<-(resi[4,1]-resi[5,1])*2791/(4*resi[5,1])
F<qf(0.95,4,2791)#FALSE
F<-(resi[5,1]-resi[6,1])*2785/(4*resi[6,1])
F<qf(0.95,4,2785)#TRUE
arima(sunspots,order=c(10,0,9))#ar10 0.3924 se 0.0283 利用t检验知最高项系数不为0
aico1=resi1<-matrix(0,nrow=9)
for (i in 1:9) {
a<-arima(sunspots,order=c(10,0,i))
aico1[i]<-a$aic
resi1[i]=sum(a$resi^2)
}
aico1
F<-(resi1[8,1]-resi1[9,1])*2785/(resi1[8,1])
F<qf(0.95,4,2785)#FALSE,说明这里ma的阶数不可以降，所以选择模型arma（10,9）
sunspots.fit2<-arima(sunspots,order=c(10,0,9))#AIC=23473.9
#从AIC的角度来看，这个模型确实更好，但也更加的繁琐，从预测来看效果也没提高多少
library(forecast)
sunspots.forecast2 <- forecast(sunspots.fit2,12)
plot.forecast(sunspots.forecast2)
#做残差检验，来判断模型
Box.test(sunspots.fit1$residuals)#ARIMA(2,1,0)   p-value = 0.1031
Box.test(sunspots.fit$residuals)#AR(2) p-value = 0.006665
Box.test(sunspots.fit2$residuals)#ARMA(10,9)  p-value = 0.9418
#综上，sunspots模型确定为ARIMA（2,1,0）最好
