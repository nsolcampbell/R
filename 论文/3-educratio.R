
load('educ-years.rdata')
head(book1)
educ.years<-book1

as.data.frame(educ.years)
head(educ.years)
names(educ.years)<-c('hhid','line','gender','wave','educyear')
aabbcc<-data.frame(educ.years$hhid,educ.years$wave,educ.years$educyear)
library(reshape)
mda<-melt(aabbcc,id=c('educ.years.hhid','educ.years.wave'))
mdmax1<-cast(mda,educ.years.hhid~educ.years.wave,max)
mdmin1<-cast(mda,educ.years.hhid~educ.years.wave,min)
head(mda)
mdasum1<-cast(mda,educ.years.hhid~educ.years.wave,sum)
head(mdmax1)
head(mdmin1)
head(mdasum1)
true.same.year.couple.2011<-mdasum1$educ.years.hhid[mdasum1$'2011'==mdmax1$'2011'*2]
true.same.year.couple.2011<-true.same.year.couple.2011[is.na(true.same.year.couple.2011)==F]
true.same.year.couple.2000<-mdasum1$educ.years.hhid[mdasum1$'2000'==mdmax1$'2000'*2]
true.same.year.couple.2000<-true.same.year.couple.2000[is.na(true.same.year.couple.2000)==F]
true.same.year.couple.1989<-mdasum1$educ.years.hhid[mdasum1$'1989'==mdmax1$'1989'*2]
true.same.year.couple.1989<-true.same.year.couple.1989[is.na(true.same.year.couple.1989)==F]


mdbi<-mdmax1/mdmin1
head(mdbi)
mdbi$educ.years.hhid<-mdmax1$educ.years.hhid

mdbi.inf.kanzuo.18<-mdbi
mdbi.inf.kanzuo.18[mdbi.inf.kanzuo.18==Inf]=18
mdbi.inf.kanzuo.18
describe(mdbi.inf.kanzuo.18)
mdbi.inf.kanzuo.18.2011<-data.frame(mdbi.inf.kanzuo.18$educ.years.hhid ,mdbi.inf.kanzuo.18$'2011')
mdbi.inf.kanzuo.18.2000<-data.frame(mdbi.inf.kanzuo.18$educ.years.hhid ,mdbi.inf.kanzuo.18$'2000')
mdbi.inf.kanzuo.18.1989<-data.frame(mdbi.inf.kanzuo.18$educ.years.hhid ,mdbi.inf.kanzuo.18$'1989')

head(mdbi.inf.kanzuo.18.2011)
head(mdbi.inf.kanzuo.18.2000)
head(mdbi.inf.kanzuo.18.1989)

describe(mdbi.inf.kanzuo.18.2011)
describe(mdbi.inf.kanzuo.18.2000)
describe(mdbi.inf.kanzuo.18.1989)

mdbi.zaikan.danqin.18<-mdbi.inf.kanzuo.18
mdbi.zaikan.danqin.18[mdbi.zaikan.danqin.18==1]=18
mdbi.zaikan.danqin.18.2011<-data.frame(mdbi.zaikan.danqin.18$educ.years.hhid ,mdbi.zaikan.danqin.18$'2011')
mdbi.zaikan.danqin.18.2000<-data.frame(mdbi.zaikan.danqin.18$educ.years.hhid ,mdbi.zaikan.danqin.18$'2000')
mdbi.zaikan.danqin.18.1989<-data.frame(mdbi.zaikan.danqin.18$educ.years.hhid ,mdbi.zaikan.danqin.18$'1989')


head(mdbi.zaikan.danqin.18.2011)
head(mdbi.zaikan.danqin.18.2000)
head(mdbi.zaikan.danqin.18.1989)

jiaoyu.bili.2011<-mdbi.zaikan.danqin.18.2011
jiaoyu.bili.2000<-mdbi.zaikan.danqin.18.2000
jiaoyu.bili.1989<-mdbi.zaikan.danqin.18.1989

for (i in true.same.year.couple.2011)
  jiaoyu.bili.2011$mdbi.zaikan.danqin.18..2011.[jiaoyu.bili.2011$mdbi.zaikan.danqin.18.educ.years.hhid==i]=1
for (i in true.same.year.couple.2000)
  jiaoyu.bili.2000$mdbi.zaikan.danqin.18..2000.[jiaoyu.bili.2000$mdbi.zaikan.danqin.18.educ.years.hhid==i]=1
for (i in true.same.year.couple.1989)
  jiaoyu.bili.1989$mdbi.zaikan.danqin.18..1989.[jiaoyu.bili.1989$mdbi.zaikan.danqin.18.educ.years.hhid==i]=1



names(jiaoyu.bili.2011)<-c('hhid','ratio')
names(jiaoyu.bili.2000)<-c('hhid','ratio')
names(jiaoyu.bili.1989)<-c('hhid','ratio')


length(jiaoyu.bili.2011$hhid)
length(jiaoyu.bili.2000$hhid)
length(jiaoyu.bili.1989$hhid)



final.2011.educ.ratio<-c()
for (i in goodid.income.2011)
  final.2011.educ.ratio<-append(final.2011.educ.ratio,jiaoyu.bili.2011$ratio[jiaoyu.bili.2011$hhid==i])
length(final.2011.educ.ratio)
head(final.2011.educ.ratio)
describe(final.2011.educ.ratio)

#for 2000
final.2000.educ.ratio<-c()
for (i in goodid.income.2000)
  final.2000.educ.ratio<-append(final.2000.educ.ratio,jiaoyu.bili.2000$ratio[jiaoyu.bili.2000$hhid==i])
length(final.2000.educ.ratio)
head(final.2000.educ.ratio)
describe(final.2000.educ.ratio)

#for 1989
final.1989.educ.ratio<-c()
for (i in goodid.income.1989)
  final.1989.educ.ratio<-append(final.1989.educ.ratio,jiaoyu.bili.1989$ratio[jiaoyu.bili.1989$hhid==i])
length(final.1989.educ.ratio)
head(final.1989.educ.ratio)
describe(final.1989.educ.ratio)



id.hhpp.chuvalue.hhsize.educratio<-data.frame(thisid.for.income,final.2011.income,final.2011.chuvalue,final.2011.size,final.2011.educ.ratio)
write.csv(id.hhpp.chuvalue.hhsize.educratio,file='id-hhpp-chuvalue-hhsize-educratio.csv')
names(id.hhpp.chuvalue.hhsize.educratio)<-c('id','hhpp','chuvalue','hhsize','educratio')
huigui.try2<-lm(id.hhpp.chuvalue.hhsize.educratio$hhpp~id.hhpp.chuvalue.hhsize.educratio$chuvalue+id.hhpp.chuvalue.hhsize.educratio$hhsize+id.hhpp.chuvalue.hhsize.educratio$educratio)
summary(huigui.try2)
plot(huigui.try2)

head(wagedata)
new.wagedata<-data.frame(wagedata$hhid,wagedata$wave,wagedata$b2d,wagedata$c8,wagedata$i19)
names(new.wagedata)<-c('hhid','wave','retire','wage','jiangjin')
new.wagedata$hhid[which(new.wagedata$wage<0)]
new.wagedata[new.wagedata==-9]=NA
new.wagedata[new.wagedata==-99]=NA
new.wagedata[new.wagedata==-999]=NA
new.wagedata[new.wagedata==-9999]=NA

sum.of.wagedata<-new.wagedata$retire+new.wagedata$wage+new.wagedata$jiangjin
describe(sum.of.wagedata)
new.wagedata$hhid[which(new.wagedata$wage==-20997)]

load('hhrelationship.rdata')
head(hhrelationship)
names(hhrelationship)<-c('hhid','peiou','line','wave')
newhhrelat<-hhrelationship[is.na(hhrelationship$peiou)==F,]
newhhrelat<-newhhrelat[newhhrelat$peiou>2,]
newhhrelat
head(newhhrelat)

zhe.ge.list.you.duo.ren<-newhhrelat
zhe.ge.list.you.duo.ren.2011<-newhhrelat[wave==2011,]
head(zhe.ge.list.you.duo.ren.2011)
id.need.review<-c()
for (i in thisid.for.income)
  id.need.review<-append(id.need.review,zhe.ge.list.you.duo.ren.2011$hhid[zhe.ge.list.you.duo.ren.2011$hhid==i])

