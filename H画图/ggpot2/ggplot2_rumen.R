library(ggplot2)
data(mpg)
# 按颜色分类，变量为离散变量
qplot(displ, hwy, colour = class, data = mpg)

# 当颜色分类变量为连续变量时
qplot(displ, hwy, colour = cty , data = mpg)

# 调整size和shape（不支持连续变量）
qplot(displ, hwy, size = class, data = mpg)
qplot(displ, hwy, shape = class, data = mpg)

#size按属性变量class区分，colour按属性变量trans区分
qplot(displ, hwy, size = class, colour = trans, data = mpg)


# 加号表示增加图层
#facet_grid(): 2-dimension grid, rows ~ cols,
qplot(displ, hwy, data = mpg) +
facet_grid(. ~ cyl)

qplot(displ, hwy, data = mpg) +
facet_grid(drv ~ .)

qplot(displ, hwy, data = mpg) +
facet_grid(drv ~ cyl)

按一个类别进行分类的散点图

# facet_wrap(): 1-dimension grid
qplot(displ, hwy, data = mpg) +
facet_wrap(~ class)


#reorder()在图形上自动实现按某些变量的排序


#按某些指标自动排序的散点图

#reorder(class,hwy)，自动调整属性变量class的排序，使得类别的hwy从小到大
qplot(reorder(class, hwy), hwy, data = mpg)


#按色彩分类与箱线图，加入随机扰动（为了防止出现多个点在一点覆盖的现象）

# geom = c("jitter", "boxplot")， jitter增加随机扰动（显示重叠的点），boxplot绘制箱线图
qplot(reorder(class,hwy), hwy, data = mpg, geom = c("jitter", "boxplot"),colour=class)

 

#参考文献：Hadley Wickham《ggplot2: Elegant Graphics for Data Analysis》第二章

#qplot()函数的详细用法：

library(ggplot2)

# 测试数据集，ggplot2内置的钻石数据
qplot(carat, price, data = diamonds)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]   #对diamonds数据集进行抽样

#1. 按color,size,shape的基本分类可视化

#1.1 简单的散点图（利用color分类，不同颜色的钻石由不同颜色的点代表）
qplot(carat, price, data = dsmall, colour = color)

 

#1.2. 简单的散点图（利用shape分类，不同的切割方式由不同形状的点代表）
qplot(carat, price, data = dsmall, shape = cut)

 

#2. 绘制不同类型的图表：geom参数

qplot(x,y,data=data,geom="")中的geom=""用来控制输出的图形类型
I. 两变量图
(1) geom="points"，默认参数，绘制散点图(x,y)
(2) geom="smooth" 绘制平滑曲线（基于loess, gam, lm ,rlm,glm）
(3) geom="boxplot" 绘制箱线图 ，当x为属性变量(factor)，y为数值变量时

II.单变量图
(4) geom="histogram"，直方图
(5) geom="density"，核密度估计图
(6) geom="bar"，条形图barchart

III.时间序列
(7) geom="line"，折线图，可用于时间序列（当x=date)
(8) geom="path"，路径图（参见后文）


# 2.1 同时绘制散点图+平滑直线
qplot(carat, price, data = dsmall, geom=c("point","smooth"))

 

#参数调整：method=""等
#(a). method = "loess", 默认平滑算法,  通过span=调整窗宽， span=0(波动) 到 span=1（光滑）
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "loess",span=0.2)

 

# (b). method = "gam": GAM 在大数据时比loess高效，需要载入 mgcv 包
library(mgcv)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method="gam", formula = y ~ s(x))

# (c). method="lm", 线性平滑
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
     method = "lm")

# method="lm",formula = y ~ ns(x, 3)，三次自然样条，需要载入splines包
library(splines)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "lm", formula = y ~ ns(x, 3))

# method = "rlm", robust linear model, 受异常值影响小，需要载入MASS包
library(MASS)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      method = "rlm")


# 2.2：x为属性变量，y为连续变量，绘制boxplot
qplot(color, price/carat, data=diamonds,geom="boxplot")

 

# 2.3：单变量，直方图
qplot(carat, data = diamonds, geom = "histogram")

 

#2.4: 单变量，核密度估计图
qplot(carat, data = diamonds, geom = "density")

 

# 按不同颜色绘制的density图
require(ggplot2)
data(diamonds)
qplot(carat, data = diamonds, geom = "density",colour=color)

 

# 2.5 条形图（柱状图）
#计数，求count(color)
qplot(color, data = diamonds, geom = "bar") 

#加权,对每个求sum(carat)，类似于excel里的数据透视图，按不同的color计算carat的总和
qplot(color, data = diamonds, geom = "bar", weight = carat)  

 


#2.6. Time-series
qplot(date, unemploy / pop, data = economics, geom = "line")

 


#2.7. Path plot
#如果要查看失业率(unemploy / pop)与平均失业时间(uempmed)之间的关系，一个方法是利用散点图，但是这样做就会导致无法观察到随时间变化的趋势了，path plot利用颜色深浅来代表年份，随着颜色从浅蓝变成深蓝，可以观察到失业率与失业时间的关系的变化趋势。

#具体实现：先自定义函数year(),将字符串格式的时间转化为年
year <- function(x) as.POSIXlt(x)$year + 1900 

#画出path plot,颜色按年份由浅到深
qplot(unemploy / pop, uempmed, data = economics,
      geom = "path", colour = year(date))