#《Economist》（经济学人）是一份由伦敦经济学人报纸有限公司出版的杂志,
#于1843年9月由詹姆士·威尔逊创办。杂文章写得机智，幽默，有力度，严肃
#又不失诙谐，并且注重于如何在最小的篇幅内告诉读者最多的信息。
#杂志主要关注政治和商业方面的新闻，但是每期也有一两篇针对科技和
#艺术的报导，以及一些书评。从2012年1月28日的那一期杂志开始，
#《经济学人》杂志开辟了中国专栏，为有关中国的文章提供更多的版面。

#杂志不仅文章内容精彩，排版和图片也非常具有特色。
#其中的统计图片均是以蓝色为基调，简洁而大气。
#如果你想绘制出这种风格的统计图，现在也很简单了。
#只需要使用R语言中大名鼎鼎的ggplot2包，以及Jeffrey Arnold新开发
#的ggthemes包，就能轻易的实现了。

#ggthemes包还不能从CRAN库中安装，所以需要从github上下载原始文档，
#编译成zip本地安装包，最后在R中进行安装加载就可以使用了。
#如果你已经安装了Rtools，也可以使用devtools包中
#的install_github()函数来直接安装github上的R包。

#下面就是用ggplot2+ggthemes包作的一个小例子，
#其中重要的就是要用到theme_economist()和scale_color_economist()
#这两个函数来实现economist风格的统计图。很简单吧！ggthemes包中
#还有其它主题可供使用，慢慢研究。
install.packages("Rtools")
install.packages("devtools")
library(devtools)
install_github("ggthemes")

p <- ggplot(data=mpg, mapping=aes(x=cty,y=hwy))
p + geom_point(aes(color=factor(year)),
               alpha=0.5,position = "jitter")+
    stat_smooth()+
    theme_economist() + scale_color_economist()+
    labs(title = '汽车型号与油耗',
         y = '每加仑高速公路行驶距离',
         x = '每加仑城市公路行驶距离',
         colour = '年份')