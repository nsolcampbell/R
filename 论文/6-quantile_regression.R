#重新整理2011
data.2011<-data.frame(final.2011.income,goodid.income.2011,final.2011.chuvalue,final.2011.size,final.2011.educ.ratio)
data.2011.income<-c()
data.2011.chuvalue<-c()
data.2011.size<-c()
data.2011.educ.ratio<-c()
data.2011.income.ratio<-final.2011.income.ratio
for (i in final.2011.id.yes)
{
  data.2011.income<-append(data.2011.income,data.2011$final.2011.income[data.2011$goodid.income.2011==i])
  data.2011.chuvalue<-append(data.2011.chuvalue,data.2011$final.2011.chuvalue[data.2011$goodid.income.2011==i])
  data.2011.size<-append(data.2011.size,data.2011$final.2011.size[data.2011$goodid.income.2011==i])
  data.2011.educ.ratio<-append(data.2011.educ.ratio,data.2011$final.2011.educ.ratio[data.2011$goodid.income.2011==i])
}
length(data.2011.income)
length(data.2011.chuvalue)
length(data.2011.size)
length(data.2011.educ.ratio)

#重新整理2000
data.2000<-data.frame(final.2000.income,goodid.income.2000,final.2000.chuvalue,final.2000.size,final.2000.educ.ratio)
data.2000.income<-c()
data.2000.chuvalue<-c()
data.2000.size<-c()
data.2000.educ.ratio<-c()
data.2000.income.ratio<-final.2000.income.ratio
for (i in final.2000.id.yes)
{
  data.2000.income<-append(data.2000.income,data.2000$final.2000.income[data.2000$goodid.income.2000==i])
  data.2000.chuvalue<-append(data.2000.chuvalue,data.2000$final.2000.chuvalue[data.2000$goodid.income.2000==i])
  data.2000.size<-append(data.2000.size,data.2000$final.2000.size[data.2000$goodid.income.2000==i])
  data.2000.educ.ratio<-append(data.2000.educ.ratio,data.2000$final.2000.educ.ratio[data.2000$goodid.income.2000==i])
}
length(data.2000.income)
length(data.2000.chuvalue)
length(data.2000.size)
length(data.2000.educ.ratio)


#重新整理1989
data.1989<-data.frame(final.1989.income,goodid.income.1989,final.1989.chuvalue,final.1989.size,final.1989.educ.ratio)
data.1989.income<-c()
data.1989.chuvalue<-c()
data.1989.size<-c()
data.1989.educ.ratio<-c()
data.1989.income.ratio<-final.1989.income.ratio
for (i in final.1989.id.yes)
{
  data.1989.income<-append(data.1989.income,data.1989$final.1989.income[data.1989$goodid.income.1989==i])
  data.1989.chuvalue<-append(data.1989.chuvalue,data.1989$final.1989.chuvalue[data.1989$goodid.income.1989==i])
  data.1989.size<-append(data.1989.size,data.1989$final.1989.size[data.1989$goodid.income.1989==i])
  data.1989.educ.ratio<-append(data.1989.educ.ratio,data.1989$final.1989.educ.ratio[data.1989$goodid.income.1989==i])
}
length(data.1989.income)
length(data.1989.chuvalue)
length(data.1989.size)
length(data.1989.educ.ratio)







#关于分位数回归
#install.packages('quantreg')
library(quantreg)
#建立方程形式
#quantreg.2011= hhp ~ experience + I(experience^2) + education
data.2011<-data.frame(data.2011.income,final.2011.id.yes,data.2011.chuvalue,data.2011.size,data.2011.educ.ratio,data.2011.income.ratio)
names(data.2011)<-c('income','hhid','agestructure','hhsize','educratio','wageratio')
head(data.2011)
#data.2011->beiyong.data.2011
#data.2011<-na.omit(data.2011)
#id.hhpp.chuvalue.hhsize.educratio<-read.csv('id-hhpp-chuvalue-hhsize-educratio.csv')
#id.hhpp.chuvalue.hhsize.educratio<-data.frame(thisid.for.income,final.2011.income,
#                                              final.2011.chuvalue,final.2011.size,
#                                              final.2011.educ.ratio)
#san.bian.liang<-id.hhpp.chuvalue.hhsize.educratio
#san.bian.liang<-na.omit(san.bian.liang)
#san.bian.liang<-san.bian.liang[san.bian.liang$income>0,]
#describe(san.bian.liang)
quantreg.2011=log(income)~agestructure+hhsize+educratio+wageratio
si.bian.liang.2011<-data.2011
#利用rq命令回归，默认为0.5分位数
si.bian.liang.2011<-data.2011
si.bian.liang.2011<-na.omit(si.bian.liang.2011)
si.bian.liang.2011<-si.bian.liang.2011[si.bian.liang.2011$income>0,]
describe(si.bian.liang.2011)

cps_lad= rq(quantreg.2011, data = si.bian.liang.2011)
summary(cps_lad)

#用多个分位数进行回归
cps_rqbig= rq(quantreg.2011, tau = seq(0.05, 0.95, by = 0.05),
              data = si.bian.liang.2011)
cps_rqbigs = summary(cps_rqbig)
cps_rqbigs
#绘制图形
plot(cps_rqbigs) 










data.2000<-data.frame(data.2000.income,final.2000.id.yes,data.2000.chuvalue,data.2000.size,data.2000.educ.ratio,data.2000.income.ratio)
names(data.2000)<-c('income','hhid','agestructure','hhsize','educratio','wageratio')
head(data.2000)

quantreg.2000=log(income)~agestructure+hhsize+educratio+wageratio
si.bian.liang.2000<-data.2000
#利用rq命令回归，默认为0.5分位数
si.bian.liang.2000<-data.2000
si.bian.liang.2000<-na.omit(si.bian.liang.2000)
si.bian.liang.2000<-si.bian.liang.2000[si.bian.liang.2000$income>0,]
describe(si.bian.liang.2000)

cps_lad= rq(quantreg.2000, data = si.bian.liang.2000)
summary(cps_lad)

#用多个分位数进行回归
cps_rqbig= rq(quantreg.2000, tau = seq(0.05, 0.95, by = 0.05),
              data = si.bian.liang.2000)
cps_rqbigs = summary(cps_rqbig)
cps_rqbigs
#绘制图形
plot(cps_rqbigs) 








data.1989<-data.frame(data.1989.income,final.1989.id.yes,data.1989.chuvalue,data.1989.size,data.1989.educ.ratio,data.1989.income.ratio)
names(data.1989)<-c('income','hhid','agestructure','hhsize','educratio','wageratio')
head(data.1989)
#data.1989->beiyong.data.1989
#data.1989<-na.omit(data.1989)
#id.hhpp.chuvalue.hhsize.educratio<-read.csv('id-hhpp-chuvalue-hhsize-educratio.csv')
#id.hhpp.chuvalue.hhsize.educratio<-data.frame(thisid.for.income,final.1989.income,
#                                              final.1989.chuvalue,final.1989.size,
#                                              final.1989.educ.ratio)
#san.bian.liang<-id.hhpp.chuvalue.hhsize.educratio
#san.bian.liang<-na.omit(san.bian.liang)
#san.bian.liang<-san.bian.liang[san.bian.liang$income>0,]
#describe(san.bian.liang)
quantreg.1989=log(income)~agestructure+hhsize+educratio+wageratio
si.bian.liang.1989<-data.1989
#利用rq命令回归，默认为0.5分位数
si.bian.liang.1989<-data.1989
si.bian.liang.1989<-na.omit(si.bian.liang.1989)
si.bian.liang.1989<-si.bian.liang.1989[si.bian.liang.1989$income>0,]
describe(si.bian.liang.1989)

cps_lad= rq(quantreg.1989, data = si.bian.liang.1989)
summary(cps_lad)

#用多个分位数进行回归
cps_rqbig= rq(quantreg.1989, tau = seq(0.05, 0.95, by = 0.05),
              data = si.bian.liang.1989)
cps_rqbigs = summary(cps_rqbig)
cps_rqbigs
#绘制图形
plot(cps_rqbigs) 


write.csv(data.2011,'data-2011.csv')
write.csv(data.2000,'data-2000.csv')
write.csv(data.1989,'data-1989.csv')