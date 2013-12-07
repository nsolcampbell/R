#ARMA模型的参数估计方法
#             ARMA参数估计和前面我们介绍的点估计内容相似，也介绍矩估计与最小二乘估计两种方法。
#           和上一次的点估计一样，这一次我分享的内容主要有：矩估计，最小二乘估计，一个应用例题
#            关于矩估计与最小二乘估计的基本思想，参见前面点估计的有关介绍.
#           A RMA 模型（Auto-Regressive and Moving Average Model）是研究时间序列的重要方法，由自回归模型（简称AR模型）与滑动平均模型（简称MA模型）为基础“混合”构成。在市场研究中常用于长期追踪资料的研究，如：Panel研究中，用于消费行为模式变迁研究；在零售研究中，用于具有季节变动特征的销售量、市场规模的预测等。
#           ARMA模型的基本原理：将预测指标随时间推移而形成的数据序列看作是一个随机序列，这组随机变量所具有的依存关系体现着原始数据在时间上的延续性。一方面，影响因素的影响，另一方面，又有自身变动规律，假定影响因素为x1，x2，…，xk，由回归分析， 　　 
#           对于arma模型的矩估计和最小二乘估计需要了解yule-walker方程。你可以参阅：http://blog.sina.com.cn/s/blog_4b700c4c0102e728.html去了解关于yule-walker方程的内容。
#          我在这里只介绍两个根据数据直接估计出ar,arma,模型的函数:ar()，arima().其中ar(),arima()是扩展包TSA中的函数，如果要调用，你需要安装并加载这个扩展包。
#先看ar()的用法：
ar(x, aic = TRUE, order.max = NULL,
  method=c("yule-walker", "burg", "ols","mle", "yw"),
  na.action, series, ...)
#这里如果是对ar模型做矩估计，method选择“yw”，极大似然估计使用”mle“。
#我们来看一个例子：
#还是用我们在上一篇博文中提到的生成ar模型数据的办法生成一批数据，然后对他们做矩估计。
w<-rnorm(550)
x<-filter(w,filter=c(1,-0.9),"recursive")
ar(x,order=2,method="yw")
 
#Call:
#ar(x = x,order.max = 2, method = "yw")
 
#Coefficients:
#      1       2 
# 0.9870 -0.8905 
 
#Orderselected 2  sigma^2 estimated as  1.085
#对比系数，发现这个估计是十分接近的。这是因为对ar模型的矩估计利用的yule-waler方程可以看做线性方程，从而估计效果十分良好。可以对比最小二乘估计：
ar(x,order=2,method="ols")
 
#Call:
#ar(x = x, order.max = 2, method ="ols")
 
#Coefficients:
#     1        2 
# 0.9953 -0.8988 
 
#Intercept: -0.003315 (0.04332)
 
#Order selected 2  sigma^2 estimated as  1.028
#可以看出估计效果差不多，同样的，可以选择极大似然估计，这里不再赘述。
 
#对于ma模型，必须指出的是无论是最小二乘，极大似然估计，还是矩估计，效果都是不理想的。在R中除了可以令arma(0,q)表示ma模型
#外（arma，arima提供的方法都是比较精细的，像矩估计这样粗糙的方法是没被采纳的），我也没有找到ma模型的矩估计函数（可能是因为效
#果不好。所以要用只有自己编写的了）。
#对于arma模型，可以使用函数arima(),arma().去做参数估计。这里将两个函数的用法摘录如下：
#arma(x, order = c(1, 1), lag = NULL, coef = NULL,
#     include.intercept = TRUE, series = NULL, qr.tol = 1e-07, ...)
#arima(x, order = c(0, 0, 0),
#      seasonal = list(order = c(0, 0, 0), period = NA),
#      xreg = NULL, include.mean = TRUE,
#      transform.pars = TRUE,
#      fixed = NULL, init = NULL,
#      method = c("CSS-ML", "ML", "CSS"),
#      n.cond, optim.method = "BFGS",
#      optim.control = list(), kappa = 1e6)
#最后举一例来说明arma的参数估计：
#还是用太阳黑子的数据来说明问题：
data(sunspots)
arma(sunspots,order=c(2,1))
 
#Call:
#arma(x =sunspots, order = c(2, 1))
# 
#Coefficient(s):
#      ar1       ar2        ma1  intercept 
#   1.1984   -0.2116    -0.6214     0.6673  