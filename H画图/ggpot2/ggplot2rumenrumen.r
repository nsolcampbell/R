install.packages("ggplot2")
library(ggplot2)

#下面用ggplot2包内带的汽车测试数据（mpg）来举个例子，用到的三个变量分别是发动机容量(displ)、高速公路上的每加仑行驶里数(hwy)、汽缸数目(cyl)。
#首先加载ggplot2包，然后用ggplot定义第一层即数据来源。
#其中aes参数非常关键，它将displ映射到X轴，将hwy映射到Y轴，将cyl变为分类数据后映射为不同的颜色。
#然后使用+号添加了两个新的图层，第二层是加上了散点，第三层是加上了loess平滑曲线。

p <- ggplot(data=mpg,aes(x=displ,y=hwy,colour=factor(cyl)))
p + geom_point() + geom_smooth()

#上图是对几种不同汽缸的数据分别平滑，
#如果需要对整体数据进行平滑，可将colour参数设置在散点图层内而非第一层，这样第三层的平滑图形就不会受到colour参数

p <- ggplot(mpg,aes(x=displ,y=hwy))
p + geom_point(aes(colour=factor(cyl))) + geom_smooth()


#其它资源：
#http://had.co.nz/ggplot2/ （官方主站）
#http://xccds1977.blogspot.com/2011/12/qplot.html （快速入门）
#ggplot2 Elegant Graphics for Data Analysis （下载）