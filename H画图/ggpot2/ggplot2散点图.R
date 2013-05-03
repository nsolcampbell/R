#1 色彩和形状的控制
#数据特征不仅可以用坐标来表示，也可以用不同的色彩或形状来表示。
#仍以mpg数据集为例，所用到的变量有cty（城市中行驶距离）,hwy（高速路行驶距离）,displ（排量大小）,year（生产年份）

library(ggplot2)
p <- ggplot(mpg, aes(cty, hwy))
p1<- p + geom_point(aes(colour = factor(year),shape = factor(year), size = displ), alpha = 0.6, position = ‘jitter’)
print(p1)

#我们将1999年生产车型用红色圆形表示，2008年用兰色三角形表示，排量用图形的大小表示，并且设置了透明度和jitter以避免样本点之间的重叠。
#可观察到2008年生产的大排量车型较多，从而油耗较高，单位油耗行驶距离较短。


#2 坐标的控制
#上图右上角数据点较为稀疏，这种情况下可用对数变换。为了演示ggplot2对图形坐标的控制，我们对X轴和Y轴均进行对数变换，
#然后对X轴的坐标显示加以限制，只显示X轴数据的均值，以及一倍标准差的坐标。

cty.mean=with(mpg,mean(cty))
cty.sd=with(mpg,sd(cty))
p1 + scale_x_continuous(trans=’log’,breaks=c(cty.mean-cty.sd,cty.mean,cty.mean+cty.sd), labels=c(“high”, “mean”, “low”)) + scale_y_continuous(trans=’log’)


#3 文字说明
#利用geom_text函数可添加文字说明以增强图形的可读性

p <- ggplot(mtcars, aes(x=wt, y=mpg,colour=factor(cyl),label=rownames(mtcars)))
p + geom_text(hjust=0,vjust=-1,alpha=0.8)+ geom_point(size=3,aes(shape=factor(cyl)))


#4 矩阵散点图
#ggplot2包中也提供了矩阵散点图函数

plotmatrix(USArrests)+geom_smooth()