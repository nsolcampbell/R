
#R包管理

#1.R的包数量查询
a<-available.packages()
length(a)
#[1] 95251

##1.1批量安装R包
install.pacakges(c(‘slidify’,’ggplot2’,’devtools’))

source(‘****/**.R')

search()#得到全部可用的口令
