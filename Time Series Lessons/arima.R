#############################################################
####  使用差分 log 对不稳定序列进行“稳定化”并计算d     ####
#############################################################
# 取差分，使其稳定化：
#代码：
#   diff(data)#1阶差分，d=1
#   diff(data,2)#2阶差分，d=2
# 取log再差点，使其稳定化：
#代码：
#   diff(log(data))#1阶差分，d=1
#   diff(log(data),2)#2阶差分，d=2
# 此处不做代码的实验，各位可以自己尝试
 
###############################################
####    通过acf图 pacf图 eacf来计算p q     ####
###############################################
#### step1 通过acf和pacf确定序列的类型：
#              acf                                        pacf
#   MA(q)      lag>=q+1的自相关系数全部约小于0   |     逐步将接近0
#   AR(p)      逐步将接近0                       |     lag>=p+1的自相关系数全部约小于0  
#   ARMA(p,q)  逐步将接近0                       |     逐步将接近0
#### step2 序列的类型确定后：求p q
#   如果是MA(q) 类型的，看acf图，看前几个不为0
#   如果是AR(p) 类型的，看pacf图，看前几个不为0
#   如果是ARMA(p,q) 类型的，看eacf图，看x三角形的顶端
#代码：data是向量数据
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
data<-y.arma(a1=0.92,a2=0.65,b1=0.92,b2=-0.65,num=100,pic=F)
plot.cf(data)
 
###############################################
####         产生ariam模型                 ####
###############################################
# p AR的参数
# q MA的参数
# d 移动取差的阶数
# arima(data.ts,order=c(p,d,q))#产生模型。arima只处理时间序列，不处理向量等
p=2;d=0;q=2
data<-y.arma(a1=0.92,a2=0.65,b1=0.92,b2=-0.65,num=100,pic=F)
data.ts<-ts(data,freq=1,star=1)#把向量化为时间序列
sol<-arima(data.ts,order=c(p,d,q))#产生模型。arima只处理时间序列，不处理向量等
dev.new()
plot(sol,n.ahead=30)#预测30个点
 
###############################################
####       检验预测模型的性能              ####
###############################################
#### 序列的正态分布验证函数
norm.test<-function(input.data,alpha=0.05,pic=TRUE){
    if(pic==TRUE){#画图形
        dev.new()
        par(mfrow=c(2,1))
        qqnorm(input.data,main="qq图")
        qqline(input.data)
        hist(input.data,freq=F,main="直方图和密度估计曲线")
        lines(density(input.data),col="blue")#密度估计曲线
        x<-c(round(min(input.data)):round(max(input.data)))
        lines(x,dnorm(x,mean(input.data),sd(input.data)),col="red")#正态分布曲线
    }
    sol<-shapiro.test(input.data)
    if(sol$p.value>alpha){
        print(paste("success:服从正态分布，p.value=",sol$p.value,">",alpha))
    }else{
        print(paste("error:不服从正态分布，p.value=",sol$p.value,"<=",alpha))    
    }
    sol
}
#####残差分析
#方案一（自己编写的，不一定好）
# ariam.sol由ariam()产生的模型
aya.res<-function(ariam.sol){
    #step1 ：检验正太性，如果符合正太，表示均值相同：稳定的
    dev.new()
    norm.test(ariam.sol$residuals)#画出QQ图,并给出检验正态分布的测试结果
    par(mfrow=c(3,1))
    #step2 ：查看时序，如果有趋势变化，则mean不为相同，即不是稳定的
    plot(ariam.sol$residuals,main="残差序列图",type="o")#画出时序图
    abline(h=0)
    #step3 ：查看acf图，如果全部在虚线内，则表示残差和时间无关系
    acf(ariam.sol$residuals, 30,main="残差的自相关图:如果所有lag的系数均处于虚线内，则原始模型较好")
    #step4 ：使用Ljung来进行白噪声验证：表示均值全部为0，即残差很小
    test.p<-0
    for(i in 1:20){
        test.p[i]<-Box.test(sol$residuals,i,fitdf=2,type="Ljung")$p.value#选取残差中lag=1--i的自相关系数来建立Ljung量。
    }
    print(test.p)
    plot(c(NA,NA,test.p[-c(1:2)]),main="残差的Ljung检验p.value:如果lag=3---以后的所有点均大于0.05，则是白噪声",ylim=c(0,1))
    abline(h=0.05,col="red")  
}
aya.res(sol)
 
#方案二(好，书上的)
#step1 ：检验正太性，如果符合正太，表示均值相同：稳定的
dev.new()
norm.test(sol$residuals)#画出QQ图,并给出检验正态分布的测试结果
dev.new()
#使用对arima产生的模型，使用tsdiag()函数，直接画出：残差的时序图，残差的acf图，Ljung产生的p.value（和0.05比较）
tsdiag(sol,gof=15,omit.initial=F)# sol是由ariam()产生的模型
 
 
###############################################
####         使用ariam模型进行预测         ####
###############################################
# arima.pred函数：
# ariam.sol由ariam()产生的模型
# nahead预测的点数
# alpha计算预测区间时的置信度：1-alpha
# 输出的数据为数据框：$pred是预测的估值
#                     $max是预测的估值的最大值（1-alpha置信度，双侧估计）
#                     $min是预测的估值的最小值（1-alpha置信度，双侧估计）
arima.pred<-function(ariam.sol,nahead=5,alpha=0.05){
    pred<-predict(ariam.sol,n.ahead=nahead)#产生nahead个预测数据，包括预测的平均值$pred.和预测标准误$se
    k<-qnorm((1-alpha/2),0,1)
    pred.data<-data.frame(pred=pred$pred,max<-pred$pred+k*pred$se,min<-pred$pred-k*pred$se)#95%的pred区间值（双侧估计）
    pred.data
}
 
data<-y.arma(a1=0.92,a2=0.65,b1=0.92,b2=-0.65,num=100,pic=F)
sol1<-arima(data.ts,order=c(2,0,2))
arma.pred<-arima.pred(sol1,nahead=30)