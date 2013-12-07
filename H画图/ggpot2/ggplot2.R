
#下面我们来绘制一个直方图作为示例。数据集仍采取mpg，对hwy变量绘制直方图。首先加载了扩展包，然后用ggplot函数建立了第一层，hwy数据映射到X轴上；
#使用+号增加了第二层，即直方图对象层。此时p被视为一种层对象，使用summary函数可得到关于它的更多信息，print(p)命令即可进行绘图。

library(ggplot2)
p <- ggplot(data = mpg,aes(x = hwy)) #aes用来定义坐标轴，x代表什么轴
p <- p + geom_histogram()
summary(p)
#data: manufacturer, model, displ, year, cyl, trans,
#  drv, cty, hwy, fl, class [234x11]
#mapping:  x = hwy
#faceting: facet_grid(. ~ ., FALSE)
#-----------------------------------
#geom_histogram:
#stat_bin:
#position_stack: (width = NULL, height = NULL)


#上面的信息告诉我们，p对象含有两层，第一层数据层描述了变量和映射方式，第二层是直方图对象（geom_histogram），
#geom表示几何对象，它是ggplot中重要的图层控制对象，因为它负责图形渲染的类型。geom_histogram是图形渲染类型的一种，其它类型可参见官网。

#每个geom对象都需要有数据输入，数据可以从第一层中自动读取，也可以在aes参数中直接设置。
#而且每个geom还默认搭配某种统计变换（stat），geom_histogram的默认统计变换是stat_bin。它负责对数据进行分组计数。

#下面我们尝试两种更为复杂的直方图，首先将数据按照year这个变量划分为两组，用不同的颜色绘制直方图，
#而且用频率而非计数来刻画Y轴，并添加密度曲线。
p <- ggplot(mpg,aes(hwy))
p + geom_histogram(position = 'identity',
    alpha=0.5,
    aes(y = ..density..,
    fill = factor(year))) +
    stat_density(geom = 'line',
    position = 'identity',
    aes(colour = factor(year)))
	
	
#画直方图——
library(ggplot2)
with(mpg,table(class,year))
p <- ggplot(data=mpg,aes(x=class,fill=factor(year)))
#p + geom_bar(position='dodge')
#p + geom_bar(position='stack')
#p + geom_bar(position='fill')
p + geom_bar(position='identity',alpha=0.3)


#可以看到dodge方式是将不同年份的数据并列放置；stack方式是将不同年份数据堆叠放置，这也是geom_bar的默认处理方式；
#fill方式和stack类似，但Y轴不再是计数，而是以百分比显示；identity方式是不做任何改变直接显示出来，所以需要设置透明度才能看得清楚。




#geom_bar是绘制条状几何对象，所以也可以用不经汇集的原始数据进行绘图。下面我们用2001到2010年间的美国GDP增长率举个例子。

y=c(1.1,1.8,2.5,3.6,3.1,2.7,1.9,-0.1,-3.5,3.0)
x=2001:2010
data=data.frame(x,y)
p=ggplot(data,aes(x,y,fill=y))
p+geom_bar(stat="identity")+ 
    geom_abline(intercept = 0, slope = 0,size=1,colour='gray')+
    geom_text(aes(label=y),hjust=0.5, vjust=-0.5 )+
    scale_y_continuous(limits=c(-3.8,4.2))+
    labs(x='年份', y='GDP增长率%')+
    opts(title = "美国GDP增长率")