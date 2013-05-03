#小例子
#生成日历
ts(matrix(c(NA,NA,NA,1:31,NA),byrow=T,5,7),frequency=7,names=c("Sun","Mon ","Tue", "Wen" ,"Thu","Fri"," Sat"))


###AR模型

#对于AR模型，我们有两种方法产生模型，例如 x（t）=x(t-1)--0.9x(t-2)+e(t)
#方法1：
w<-rnorm(550)#我们假定白噪声的分布是正态的。
x<-filter(w,filter=c(1,-0.9),"recursive")
#其中，无论是“卷积”或“递归”（可以缩写）。如果使用移动平均选择“卷积”：如果“递归”便是选择了自回归。


#方法2：  再说第二种方法：依据定义自己编程产生AR模型，还是以AR（2）模型x（t）=x(t-1)--0.9x(t-2) +e(t)为例，可编写函数如下：
w<-rnorm(550)
AR<-function(w){
x<-w
x[2]=x[1]+w[1]
for(i in 3:550)
x[i]=x[i-1]-0.9*x[i-2]+w[i]
x
}

#当然对于第二种方法产生的序列需要转换为时间序列格式，用as.ts()处理。



###MA模型
w<-rnorm(500)
v<-filter(w,sides=2,rep(1,3)/3)
#随机游走：
w<-rnorm(200)
x<-cumsum(w) #累计求和，seeexample：cumsum(1:!0)
wd<-w+0.2
xd<-cumsum(wd)



###季节性模型

#模型1
#最简单的季节模型就是一个分段的周期函数。比如说某地区一年的气温就是一个季节性模型。
#利用TSA包里给出的数据tempdub我们可以发现他就是这样的模型给出验证：
library(TSA)
data(tempdub)
month<-season(tempdub)
model1<-lm(tempdub~month)
summary(model1)
#这里结果中2月份系数表明了一月份平均气温与二月份平均气温的差异，以此类推。

#模型2
#余弦趋势μ1=βcos（2pi*f*t+φ）
#还是考虑上面气温的例子：验证：
har<-harmonic(tempdub,1)
model2<-lm(tempdub~har)
summary(model2)
#看看结果示例：
#Call:
#lm(formula = tempdub ~har)
 
#Residuals:
#     Min      1Q   Median       3Q     Max
#-11.1580  -2.2756 -0.1457   2.3754  11.2671
 
#Coefficients:
#               Estimate Std. Error t valuePr(>|t|)   
#(Intercept)     46.2660    0.3088 149.816  < 2e-16 ***
#harcos(2*pi*t)-26.7079     0.4367 -61.154  < 2e-16 ***
#harsin(2*pi*t)  -2.1697    0.4367  -4.968 1.93e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’1
 
#Residual standarderror: 3.706 on 141 degrees of freedom
#Multiple R-squared:0.9639,     Adjusted R-squared: 0.9634
#F-statistic:  1882 on 2 and 141 DF,  p-value: < 2.2e-16
#我们可以作图来看拟合效果：
 
 
##顺便指出季节模型也可以模拟：
#比如μ1=βcos（2pi*f*t+φ）模型可以模拟如下：
t<-1:500
w<-rnorm(500)
c<-2*cos(2*pi*t/50+0.6*pi+w)




###自相关与偏自相关
#给出自相关的定义：在信息分析中，通常将自相关函数称之为自协方差方程。 用来描述信息在不同时间的，信息函数值的相关性。详情可参见wiki：http://zh.wikipedia.org/wiki/%E8%87%AA%E7%9B%B8%E5%85%B3
#我们可以根据定义给出自相关系数（ACF）的算法：
#例如数据：
x<-1:10
u<-mean(x)
v<-var(x)
sum((x[1:9]-u)*(x[2:10]-u))/(9*v) #延迟1
sum((x[1:8]-u)*(x[3:10]-u))/(9*v)  #延迟2
sum((x[1:7]-u)*(x[4:10]-u))/(9*v)  #延迟3
#在R中也提供了直接计算acf的函数acf（），利用该函数也计算1至3阶的acf，结果如下：
a<-acf(x,3)
a

#Autocorrelationsof series ‘x’, by lag
 
#    0    1     2     3
#1.0000.700 0.412 0.148
#可以看出，是一样的。

#利用acf（）可以处理很多阶的acf，以太阳黑子数的数据集做例子：
data(sunspots)
acf(sunspots)  #给出了相应的图形
a<-acf(sunspots,6)  #为下面做估计做铺垫，列出前6阶的acf
a
 
#Autocorrelationsof series ‘sunspots’, by lag
 
#0.00000.0833 0.1667 0.2500 0.3333 0.4167 0.5000
# 1.000 0.922  0.890  0.875 0.864  0.850  0.836




##偏自相关：
#对于一个平稳AR(p)模型，求出滞后k自相关系数p(k)时，实际上得到并不是x(t)与x(t-k)之间单纯的相关关系。
#因为x(t)同时还会受到中间k-1个随机变量x(t-1)、x(t-2)、……、x(t-k+1)的影响，而这k-1个随机变量又都
#和x(t-k)具有相关关系，所以自相关系数p(k)里实际掺杂了其他变量对x(t)与x(t-k)的影响。 

#为了能单纯测度x(t-k)对x(t)的影响，引进偏自相关系数的概念。 
#对于平稳时间序列{x(t)}，用数学语言描述就是： 
#  p[(x(t),x(t-k)]|(x(t-1),……，x(t-k+1)={E[(x(t)-Ex(t)][x(t-k)-Ex(t-k)]}/E{[x(t-k)-Ex(t-k)]^2} 
#这就是滞后k偏自相关系数的定义。
#总之，偏自相关就是在试图解释在剔除了中间k-1个随机变量x(t-1)、x(t-2)、……、x(t-k+1)的干扰之后，x(t-k)对x(t)影响的相关程度。
#在R语言中，使用函数PACF（）可求解
#还是使用太阳黑子数的例子：

b<-pacf(sunspots,6)
b
 
#Partial autocorrelations of series ‘sunspots’, bylag
 
#0.0833 0.1667 0.2500 0.3333 0.4167 0.5000
# 0.922  0.272 0.189  0.135  0.064 0.044




###最后，我们利用这两个函数来看看AR（p）,MA(q)的自相关函数与偏自相关函数的截尾性与拖尾性。
#利用二中所介绍的方法生成AR（2），MA（2）的数据。

##AR（2）模型：
w<-rnorm(550)#我们假定白噪声的分布是正态的。
x<-filter(w,filter=c(1,-0.9),"recursive")
##MA(3)模型：
w<-rnorm(500)
v<-filter(w,sides=2,rep(1,3)/3)
qq<-pacf(x,5)
qq
 
#Partial autocorrelations of series ‘x’, by lag
#1   2    3   4    5    
# 0.532-0.861 -0.082  0.000

#可以看出AR（2）模型的偏自相关函数是截尾的（但由于这个是数据，所以出现pacf只能看出趋势，而不是在2步后直接变为0）
#对于MA（3）模型的自相关函数，由于v的第一项与最后一项缺失，不妨截取v的一部分数据，命名为a,有：
y<-acf(a,5)
y
 
#Autocorrelations of series ‘a’, by lag
 
#    0     1    2     3       4     5
#1.000   0.652   0.397   0.059  0.067  0.035
#也可以看出趋势。

