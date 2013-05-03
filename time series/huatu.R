set.seed(154)#用途是给定伪随机数的seed，在同样的seed下，R生成的伪随机数序列是相同的。
w<-rnorm(200)
x<-cumsum(w)#累计求和，seeexample：cumsum(1:!0)
wd<-w+0.2
xd<-cumsum(wd)
plot.ts(xd,ylim=c(-5,55))#plot.ts是指画time series图