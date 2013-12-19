setwd('/Users/nsol/Dropbox/手头工作/论文/DATA')
library(Hmisc)
library(reshape)
#家庭收入的数据
load('c12hhinc.rdata')
#attach(c12hhinc)
head(c12hhinc)
#install.packages('sas7bdat') #为了读取该格式的数据
#library(sas7bdat)
#read.sas7bdat('m12wed.sas7bdat')

#关于分位数回归
#install.packages('quantreg')
#library(quantreg)

#library(Hmisc)
#data1<-read.csv('1.csv')
#data2<-read.csv('2.csv')
#names(data1)<-c('perinc')
#names(data2)<-c('num')
#attach(data1);attach(data2)
#areg<-lm(perinc~num)
#summary(areg)

#agedata<-read.table('clipboard')
#install.packages("sas7bdat")
#library(sas7bdat)
#wagedata <- read.sas7bdat("m12wages.sas7bdat")
#agedata<-read.csv('age.csv')
#载入工资数据
load('m12wages.rdata')
wagedata<-m12wages
head(wagedata)
attach(wagedata)

#建立一维列连表
wage_hhid_table<-table(hhid)
head(wage_hhid_table)
wage_hhid_table>=2

#载入教育数据
load('m12educ.rdata')
educdata<-m12educ
head(educdata)

#abc<-data.frame(educdata$hhid,educdata$line,educdata$wave,educdata$age)
abc<-data.frame(educdata$hhid,educdata$wave,educdata$age)
abc
head(abc)
md<-melt(abc,id=c('educdata.hhid','educdata.wave'))
md1<-cast(md,educdata.hhid~educdata.wave,mean)
head(md1)

newabc<-abc[which(abc$educdata.age>60),]
laomd<-melt(newabc,id=c('educdata.hhid','educdata.wave'))
laomd1<-cast(laomd,educdata.hhid~educdata.wave,mean)
head(laomd1)
head(laomd)
length(laomd1$educdata.hhid)

newnewabc<-abc[which(abc$educdata.age<=60),]
xiaomd<-melt(newnewabc,id=c('educdata.hhid','educdata.wave'))
xiaomd1<-cast(xiaomd,educdata.hhid~educdata.wave,mean)
head(xiaomd1)
head(xiaomd)
length(xiaomd1$educdata.hhid)

length(md1$educdata.hhid)

#which(xiaomd1$educdata.hhid==laomd1$educdata.hhid)
#slkfajlksjfla<-c()
#for (i in xiaomd1$educdata.hhid)
#  if (i ==laomd1$educdata.hhid)
#    slkfajlksjfla<-append(slkfajlksjfla,i)
#  length(slkfajlksjfla)

#union(xiaomd1$educdata.hhid,laomd1$educdata.hhid)
#计算2011年相同家庭id
xiao.2011.you<-data.frame(xiaomd1$educdata.hhid,xiaomd1$'2011')
xiao.2011.you<-na.omit(xiao.2011.you)
lao.2011.you<-data.frame(laomd1$educdata.hhid,laomd1$'2011')
lao.2011.you<-na.omit(lao.2011.you)
#计算2000年相同家庭id
xiao.2000.you<-data.frame(xiaomd1$educdata.hhid,xiaomd1$'2000')
xiao.2000.you$xiaomd1..2000.[is.na(xiao.2000.you$xiaomd1..2000.)==T]=NA
xiao.2000.you<-na.omit(xiao.2000.you)
lao.2000.you<-data.frame(laomd1$educdata.hhid,laomd1$'2000')
lao.2000.you$laomd1..2000.[is.na(lao.2000.you$laomd1..2000.)==T]=NA
lao.2000.you<-na.omit(lao.2000.you)
#计算1989年相同家庭id
xiao.1989.you<-data.frame(xiaomd1$educdata.hhid,xiaomd1$'1989')
xiao.1989.you<-na.omit(xiao.1989.you)
lao.1989.you<-data.frame(laomd1$educdata.hhid,laomd1$'1989')
lao.1989.you<-na.omit(lao.1989.you)

#得到家庭id
#union(lao.2011.you,xiao.2011.you)
goodid.2011<-intersect(lao.2011.you$laomd1.educdata.hhid,xiao.2011.you$xiaomd1.educdata.hhid)
goodid.2000<-intersect(lao.2000.you$laomd1.educdata.hhid,xiao.2000.you$xiaomd1.educdata.hhid)
goodid.1989<-intersect(lao.1989.you$laomd1.educdata.hhid,xiao.1989.you$xiaomd1.educdata.hhid)

#goodid<-goodid.2011

#wentiid<-goodid[which(is.na(chuvalue)==T)]
#wentiid

#intersect(xiaomd1$educdata.hhid,laomd1$educdata.hhid)
#goodid<-intersect(xiaomd1$educdata.hhid,laomd1$educdata.hhid)
#head(goodid)

#for2011年
chuid.2011<-c()
chuvalue.2011<-c()
for (i in goodid.2011)
{chuchu<-laomd1$'2011'[laomd1$educdata.hhid==i]/xiaomd1$'2011'[xiaomd1$educdata.hhid==i]
 chuid.2011<-append(chuid.2011,i)
 chuvalue.2011<-append(chuvalue.2011,chuchu)}
chuvalue.2011
library(Hmisc)
describe(chuvalue.2011)

#for2000年
chuid.2000<-c()
chuvalue.2000<-c()
for (i in goodid.2000)
{chuchu<-laomd1$'2000'[laomd1$educdata.hhid==i]/xiaomd1$'2000'[xiaomd1$educdata.hhid==i]
 chuid.2000<-append(chuid.2000,i)
 chuvalue.2000<-append(chuvalue.2000,chuchu)}
library(Hmisc)
describe(chuvalue.2000)
chuvalue.2000
#which(is.na(chuvalue.2000)==T)
#chuid.2000[1]
#head(laomd1)
#laomd1$'2000'[laomd1$educdata.hhid==211101003]

#for1989年
chuid.1989<-c()
chuvalue.1989<-c()
for (i in goodid.1989)
{chuchu<-laomd1$'1989'[laomd1$educdata.hhid==i]/xiaomd1$'1989'[xiaomd1$educdata.hhid==i]
 chuid.1989<-append(chuid.1989,i)
 chuvalue.1989<-append(chuvalue.1989,chuchu)}
library(Hmisc)
describe(chuvalue.1989)
chuvalue.1989

#setdiff(A,B) 返回A 中有但B 中没有的元素
lao.y.xiao.my.2011<-setdiff(lao.2011.you$laomd1.educdata.hhid,xiao.2011.you$xiaomd1.educdata.hhid)
lao.y.xiao.my.2011
describe(lao.y.xiao.my.2011)
xiao.y.lao.my.2011<-setdiff(xiao.2011.you$xiaomd1.educdata.hhid,lao.2011.you$laomd1.educdata.hhid)
xiao.y.lao.my.2011
describe(xiao.y.lao.my.2011)

#2000返回A 中有但B 中没有的元素
lao.y.xiao.my.2000<-setdiff(lao.2000.you$laomd1.educdata.hhid,xiao.2000.you$xiaomd1.educdata.hhid)
lao.y.xiao.my.2000
describe(lao.y.xiao.my.2000)
xiao.y.lao.my.2000<-setdiff(xiao.2000.you$xiaomd1.educdata.hhid,lao.2000.you$laomd1.educdata.hhid)
xiao.y.lao.my.2000
describe(xiao.y.lao.my.2000)

#1989返回A 中有但B 中没有的元素
lao.y.xiao.my.1989<-setdiff(lao.1989.you$laomd1.educdata.hhid,xiao.1989.you$xiaomd1.educdata.hhid)
lao.y.xiao.my.1989
describe(lao.y.xiao.my.1989)
xiao.y.lao.my.1989<-setdiff(xiao.1989.you$xiaomd1.educdata.hhid,lao.1989.you$laomd1.educdata.hhid)
xiao.y.lao.my.1989
describe(xiao.y.lao.my.1989)

#得到final家庭id for2011
final.2011.id<-c()
final.2011.value<-c()
final.2011.id<-append(final.2011.id,chuid.2011)
final.2011.value<-append(final.2011.value,chuvalue.2011)
for (i in xiao.y.lao.my.2000)
{final.2011.id<-append(final.2011.id,i)
 final.2011.value<-append(final.2011.value,1)}
for (i in lao.y.xiao.my.2000)
{final.2011.id<-append(final.2011.id,i)
 final.2011.value<-append(final.2011.value,15)}
length(final.2011.id)
length(final.2011.value)

#得到final家庭id for2000
final.2000.id<-c()
final.2000.value<-c()
final.2000.id<-append(final.2000.id,chuid.2000)
final.2000.value<-append(final.2000.value,chuvalue.2000)
for (i in xiao.y.lao.my)
{final.2000.id<-append(final.2000.id,i)
 final.2000.value<-append(final.2000.value,1)}
for (i in lao.y.xiao.my)
{final.2000.id<-append(final.2000.id,i)
 final.2000.value<-append(final.2000.value,15)}

length(final.2000.id)
length(final.2000.value)

#得到final家庭id for1989
final.1989.id<-c()
final.1989.value<-c()
final.1989.id<-append(final.1989.id,chuid.1989)
final.1989.value<-append(final.1989.value,chuvalue.1989)
for (i in xiao.y.lao.my)
{final.1989.id<-append(final.1989.id,i)
 final.1989.value<-append(final.1989.value,1)}
for (i in lao.y.xiao.my)
{final.1989.id<-append(final.1989.id,i)
 final.1989.value<-append(final.1989.value,15)}
describe(final.1989.value)
length(final.1989.id)
length(final.1989.value)