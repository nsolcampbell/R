#数据挖掘数据整理
install.packages('rdatamarket')
library(rdatamarket)
#然后在Datamarket网站中搜索你需要的数据，例如中国的CPI数据，将网址复制下来，再使用dmlist命令将其转化为dataframe格式，以方便进一步处理。
data=dmlist('http://datamarket.com/data/set/148w/inflation-consumer-prices-annual#display=line&ds=148w|ga2=v.39.4f')
head(data)

#数据中默认包含了香港和澳门地区，如果你想单独研究大陆地方也很容易
newdata=split(x=data,f=data$Country)

#另一种命令dmseries则将原始数据转化为时间序列格式zoo，其使用方法也是类似的
timedata=dmseries('http://datamarket.com/data/set/148w/inflation-consumer-prices-annual#display=line&ds=148w|ga2=v.39.4f')

#如果希望获取元数据信息则使用dminfo命令
info=dminfo('http://datamarket.com/data/set/148w/inflation-consumer-prices-annual#display=line&ds=148w|ga2=v.39.4f')
print(info)