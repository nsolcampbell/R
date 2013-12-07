# 加载所需扩展包 
#install.packages("RCurl")
#install.packages("RJSONIO")
#install.packages("XML")
library(RCurl)
require(RJSONIO)
library(XML)
# 建立一个根据网址提取天气预报的子函数
fromurl<- function(finalurl) {
  # 先读取网页，再解析JSON数据存在raw中  
  web <- getURL(finalurl)
  raw <-fromJSON(web)
  high <- raw$forecast$simpleforecast$forecastday[[2]]$high['celsius']
  low <- raw$forecast$simpleforecast$forecastday[[2]]$low['celsius']
  condition <- raw$forecast$simpleforecast$forecastday[[2]]$conditions
  currenttemp <- raw$current_observation$temp_c
  currentweather <- raw$current_observation$weather
  city <- as.character(raw$current_observation$display_location['full'])
  result <-list(city=city,current=paste(currenttemp,'°C ',currentweather,sep=''),
    tomorrow=paste(high,'°C','-',low,'°C ',condition,sep=''))
  names(result) <-c('城市','当前', '明天')
  return(result)
    }
# 提取天气预报的主函数
getweather <- function(city='') {
  # 如果用户输入为空，则根据IP地址来查询
  if (city == '') {
    finalurl <- 'http://api.wunderground.com/api/yourkey/conditions/
      forecast/lang:CN/q/autoip.json'
   return(fromurl(finalurl))
  # 否则就调用google API，这时需要用XML包来解析数据得到经纬度
    } else {
    requestUrl<-paste("http://maps.googleapis.com/maps/api/geocode/xml?address="
      ,city,"&sensor=false", sep="")
   xmlResult<-xmlTreeParse(requestUrl,isURL=TRUE)
   root <- xmlRoot(xmlResult)
   lat <-xmlValue(root[['result']][['geometry']][['location']][['lat']])
   lon <-xmlValue(root[['result']][['geometry']][['location']][['lng']])
   url <- 'http://api.wunderground.com/api/yourkey/conditions/forecast/lang:CN/q/'
   # 将经纬度与其它信息相结合，形成查询地址
    finalurl <- paste(url,as.character(lat),',',as.character(lon),'.json',sep='')
   return(fromurl(finalurl))
  }
}

getweather()