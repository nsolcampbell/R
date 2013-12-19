#关于人均收入
#for 2011 
load('c12hhinc.rdata')
hhinc<-c12hhinc
head(hhinc)
hhinc.meiren.shujuji<-data.frame(hhinc$hhid,hhinc$wave,hhinc$hhincgross/hhinc$hhsize)
head(hhinc.meiren.shujuji)
names(hhinc.meiren.shujuji)<-c('hhid','wave','hhpp')

hhinc.meiren.shujuji.2011<-hhinc.meiren.shujuji[which(hhinc.meiren.shujuji$wave==2011),]
hhinc.meiren.shujuji.2011
head(hhinc.meiren.shujuji.2011)
describe(hhinc.meiren.shujuji.2011)

goodid.income.2011<-intersect(hhinc.meiren.shujuji.2011$hhid,final.2011.id)
describe(goodid.income.2011)
describe(final.2011.id)
describe(hhinc.meiren.shujuji.2011$hhid)
head(goodid.income.2011)

final.2011.income<-c()
for (i in goodid.income.2011)
  final.2011.income<-append(final.2011.income,hhinc.meiren.shujuji.2011$hhpp[hhinc.meiren.shujuji.2011$hhid==i])
final.2011.income
length(final.2011.income)
describe(final.2011.income)
final.2011.id[which(is.na(final.2011.income)==T)]

length(final.2011.income[is.na(final.2011.income)==T])
final.2011.id[which(is.na(final.2011.income)==T)]

#将2011的家庭年龄及对应id导入
guanyu.nianling.2011<-data.frame(final.2011.id,final.2011.value)
names(guanyu.nianling.2011)<-c('id','chuvalue')
#thisid.for.income.2011<-intersect(guanyu.nianling.2011$id,hhinc.meiren.shujuji.2011$hhid)
#describe(thisid.for.income.2011)
#for 2011计算人均收入
#final.2011.income<-c()
#for (i in thisid.for.income.2011)
#  final.2011.income<-append(final.2011.income,hhinc.meiren.shujuji.2000$hhpp[hhinc.meiren.shujuji.2011$hhid==i])
#final.2011.income
#length(final.2011.income)
#length(thisid.for.income.2011)

final.2011.chuvalue<-c()
A<-c()
for (i in goodid.income.2011)
  final.2011.chuvalue<-append(final.2011.chuvalue,guanyu.nianling.2011$chuvalue[guanyu.nianling.2011$id==i])
final.2011.chuvalue
describe(final.2011.chuvalue)
length(final.2011.chuvalue)
describe(goodid.income.2011)
length(goodid.income.2011)


#或者
final.2011.income<-c()
final.2011.chuvalue<-c()
goodid.income.2011<-intersect(hhinc.meiren.shujuji.2011$hhid,guanyu.nianling.2011$id)
describe(goodid.income.2011)
for (i in goodid.income.2011)
  {final.2011.income<-append(final.2011.income,hhinc.meiren.shujuji.2011$hhpp[hhinc.meiren.shujuji.2011$hhid==i])
   final.2011.chuvalue<-append(final.2011.chuvalue,guanyu.nianling.2011$chuvalue[guanyu.nianling.2011$id==i])}
 
#A<-c()
#for (i in goodid.income.2011)
#  A<-append(A,guanyu.nianling.2011$chuvalue[which(guanyu.nianling.2011$id==i)])
#final.2011.income
describe(final.2011.chuvalue)
length(final.2011.chuvalue)
length(final.2011.income)
describe(final.2011.income)
final.2011.id[which(is.na(final.2011.income)==T)]

#final.2011.chuvalue
#length(thisid.for.income.2011)



#2000年计算
hhinc.meiren.shujuji.2000<-hhinc.meiren.shujuji[which(hhinc.meiren.shujuji$wave==2000),]
describe(hhinc.meiren.shujuji.2000)
length(hhinc.meiren.shujuji.2000$hhpp)

#for 2000
#load('c12hhinc.rdata')
#hhinc<-c12hhinc
#head(hhinc)
#hhinc.meiren.shujuji<-data.frame(hhinc$hhid,hhinc$wave,hhinc$hhincgross/hhinc$hhsize)
head(hhinc.meiren.shujuji)
names(hhinc.meiren.shujuji)<-c('hhid','wave','hhpp')
hhinc.meiren.shujuji.2000<-hhinc.meiren.shujuji[which(hhinc.meiren.shujuji$wave==2000),]
hhinc.meiren.shujuji.2000
head(hhinc.meiren.shujuji.2000)
guanyu.nianling.2000<-data.frame(final.2000.id,final.2000.value)
names(guanyu.nianling.2000)<-c('id','chuvalue')
#for1989年goodid.income.2000<-intersect(final.2000.id,hhinc.meiren.shujuji.2000$hhid)
describe(goodid.income.2000)
final.2000.income<-c()
for (i in goodid.income.2000)
  final.2000.income<-append(final.2000.income,hhinc.meiren.shujuji.2000$hhpp[hhinc.meiren.shujuji.2000$hhid==i])
final.2000.income
length(final.2000.income)
final.2000.chuvalue<-c()
for (i in goodid.income.2000)
  final.2000.chuvalue<-append(final.2000.chuvalue,guanyu.nianling.2000$chuvalue[guanyu.nianling.2000$id==i])
describe(final.2000.chuvalue)

length(final.2000.chuvalue)
length(goodid.income.2000)


#for 1989
hhinc.meiren.shujuji.1989<-hhinc.meiren.shujuji[which(hhinc.meiren.shujuji$wave==1989),]
describe(hhinc.meiren.shujuji.1989)
length(hhinc.meiren.shujuji.1989$hhpp)
#load('c12hhinc.rdata')
#hhinc<-c12hhinc
#head(hhinc)
hhinc.meiren.shujuji<-data.frame(hhinc$hhid,hhinc$wave,hhinc$hhincgross/hhinc$hhsize)
head(hhinc.meiren.shujuji)
names(hhinc.meiren.shujuji)<-c('hhid','wave','hhpp')
hhinc.meiren.shujuji.1989<-hhinc.meiren.shujuji[which(hhinc.meiren.shujuji$wave==1989),]
hhinc.meiren.shujuji.1989
head(hhinc.meiren.shujuji.1989)
describe(hhinc.meiren.shujuji.1989)
guanyu.nianling.1989<-data.frame(final.1989.id,final.1989.value)
names(guanyu.nianling.1989)<-c('id','chuvalue')
thisid.for.income.1989<-intersect(guanyu.nianling.1989$id,hhinc.meiren.shujuji.1989$hhid)
describe(thisid.for.income.1989)
#for 1989
final.1989.income<-c()
for (i in thisid.for.income.1989)
  final.1989.income<-append(final.1989.income,hhinc.meiren.shujuji.1989$hhpp[hhinc.meiren.shujuji.1989$hhid==i])
final.1989.income
length(final.1989.income)
length(thisid.for.income.1989)

final.1989.chuvalue<-c()
for (i in thisid.for.income.1989)
  final.1989.chuvalue<-append(final.1989.chuvalue,guanyu.nianling.1989$chuvalue[guanyu.nianling.1989$id==i])
final.1989.chuvalue
length(final.1989.chuvalue)
length(thisid.for.income.1989)
goodid.income.1989<-thisid.for.income.1989

id.hhpp.chuvalue<-data.frame(thisid.for.income,final.2011.income,final.2011.chuvalue)
write.csv(id.hhpp.chuvalue,file='id-hhpp-chuvalue.csv')
names(id.hhpp.chuvalue)<-c('id','hhpp','chuvalue')
huigui.try<-lm(id.hhpp.chuvalue$hhpp~id.hhpp.chuvalue$chuvalue)
summary(huigui.try)
plot(huigui.try)

