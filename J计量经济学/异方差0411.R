setwd("D:\\dropbox\\Dropbox\\手头工作\\计量经济学")
library(Hmisc)
library(car)
mydata<-read.csv("1.csv")


attach(mydata);names(mydata)

scatterplotMatrix(data.frame(Consumption.Y,income.X),spread=F,main='散点图图示')

OLS<-lm(Consumption.Y~income.X)
summary(OLS)

ncvTest(OLS)
#lmeGDP<-lm(e~GDP.X)




#异方差（）版块
#Durbin-Watson检验
durbinWatsonTest(OLS)
e<-residuals(OLS)
e2<-e^2

#Breuch Pagan检验
library(lmtest)
bptest(OLS)

#得到： studentized Breusch-Pagan test

#data:  OLS
#BP = 12.0857, df = 1, p-value = 0.0005081


#画图板块
#plot(GDP.X,e)
#abline(lm(e~GDP.X))
#title('残差项对人均可支配收入的回归图')

#plot(GDP.X,e)
#title('残差项对可支配收入的散点图')


opar <- par(no.readonly = TRUE)
par(fig = c(0, 0.85, 0, 0.85))
plot(income.X,e2, xlab = "可支配收入", 
    ylab = "最小二乘回归残差项平方")
minor.tick(nx=2,ny=2,tick.ratio=0.5)
par(fig = c(0, 0.8, 0.55, 1), new = TRUE)
boxplot(income.X, horizontal = TRUE, axes = FALSE)
par(fig = c(0.65, 1, 0, 0.8), new = TRUE)
boxplot(e2, axes = FALSE)
mtext('残差项平方对可支配收入的散点图', side = 3, outer = TRUE, line = -3)
par(opar)