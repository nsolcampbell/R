load('c12hhinc.rdata')
head(c12hhinc)
hhinc<-c12hhinc
head(hhinc)
#{with hhinc
# hhinc.for.ageratio<-data.frame(hhid,wave,line,gender,)}

#names(educ.years)<-c('hhid','line','gender','wave','educyear')
head(wagedata)
new.wagedata<-data.frame(wagedata$hhid,wagedata$line,wagedata$wave,wagedata$b2d,wagedata$c8,wagedata$i19)
names(new.wagedata)<-c('hhid','line','wave','retire','wage','jiangjin')
new.wagedata$hhid[which(new.wagedata$wage<0)]
#拒答当作NA
new.wagedata[new.wagedata==-9]=NA
new.wagedata[new.wagedata==-99]=NA
new.wagedata[new.wagedata==-999]=NA
new.wagedata[new.wagedata==-9999]=NA
new.wagedata[new.wagedata==-300]=NA
describe(new.wagedata)
beiyong.wagedata<-new.wagedata
new.wagedata[is.na(new.wagedata)]=0
sum.of.wagedata<-new.wagedata$retire+new.wagedata$wage+new.wagedata$jiangjin
newnew.wagedata<-data.frame(wagedata$hhid,wagedata$line,wagedata$wave,sum.of.wagedata)
names(newnew.wagedata)<-c('hhid','line','wave','indivincome')
head(newnew.wagedata)
newnew.wagedata[newnew.wagedata==0]=NA
describe(newnew.wagedata)

#sum.of.wagedata<-new.wagedata$retire+new.wagedata$wage+new.wagedata$jiangjin
#describe(sum.of.wagedata)
#new.wagedata$hhid[which(new.wagedata$wage==-20997)]

#load('hhrelationship.rdata')
#head(hhrelationship)
#names(hhrelationship)<-c('hhid','peiou','line','wave')
#newhhrelat<-hhrelationship[is.na(hhrelationship$peiou)==F,]
#newhhrelat<-newhhrelat[newhhrelat$peiou>2,]
#newhhrelat
#head(newhhrelat)

#zhe.ge.list.you.duo.ren<-newhhrelat
#zhe.ge.list.you.duo.ren.2011<-newhhrelat[wave==2011,]
#head(zhe.ge.list.you.duo.ren.2011)
#id.need.review<-c()
#for (i in thisid.for.income)
#  id.need.review<-append(id.need.review,zhe.ge.list.you.duo.ren.2011$hhid[zhe.ge.list.you.duo.ren.2011$hhid==i])

head(newnew.wagedata)
aabbcc<-data.frame(newnew.wagedata$hhid,newnew.wagedata$wave,newnew.wagedata$indivincome)
library(reshape)
mda<-melt(aabbcc,id=c('newnew.wagedata.hhid','newnew.wagedata.wave'))
mdmax1<-cast(mda,newnew.wagedata.hhid~newnew.wagedata.wave,max)
mdmin1<-cast(mda,newnew.wagedata.hhid~newnew.wagedata.wave,min)
head(mda)
mdasum1<-cast(mda,newnew.wagedata.hhid~newnew.wagedata.wave,sum)
head(mdmax1)
head(mdmin1)
head(mdasum1)
true.same.year.couple.2011<-mdasum1$newnew.wagedata.hhid[mdasum1$'2011'==mdmax1$'2011'*2]
true.same.year.couple.2011<-true.same.year.couple.2011[is.na(true.same.year.couple.2011)==F]
true.same.year.couple.2000<-mdasum1$newnew.wagedata.hhid[mdasum1$'2000'==mdmax1$'2000'*2]
true.same.year.couple.2000<-true.same.year.couple.2000[is.na(true.same.year.couple.2000)==F]
true.same.year.couple.1989<-mdasum1$newnew.wagedata.hhid[mdasum1$'1989'==mdmax1$'1989'*2]
true.same.year.couple.1989<-true.same.year.couple.1989[is.na(true.same.year.couple.1989)==F]

#head(aabbcc)
#aaabbcc<-aabbcc[aabbcc$newnew.wagedata.wave==2011,]
#A=c()
#for (i in goodid.income.2011)
#  A=append(A,aaabbcc$newnew.wagedata.indivincome[aaabbcc$newnew.wagedata.hhid==i])
#length(A)

mdbi<-mdmax1/mdmin1
head(mdbi)
mdbi$newnew.wagedata.hhid<-mdmax1$newnew.wagedata.hhid
A=c()
for (i in goodid.income.2011)
  A=append(A,mdbi$newnew.wagedata.hhid[mdbi$newnew.wagedata.hhid==i])
length(A)


mdbi.inf.kanzuo.100<-mdbi
mdbi.inf.kanzuo.100[mdbi.inf.kanzuo.100==Inf]=100
mdbi.inf.kanzuo.100
describe(mdbi.inf.kanzuo.100)
mdbi.inf.kanzuo.100.2011<-data.frame(mdbi.inf.kanzuo.100$newnew.wagedata.hhid ,mdbi.inf.kanzuo.100$'2011')
mdbi.inf.kanzuo.100.2000<-data.frame(mdbi.inf.kanzuo.100$newnew.wagedata.hhid ,mdbi.inf.kanzuo.100$'2000')
mdbi.inf.kanzuo.100.1989<-data.frame(mdbi.inf.kanzuo.100$newnew.wagedata.hhid ,mdbi.inf.kanzuo.100$'1989')

#names(mdbi.inf.kanzuo.100.2011)<-c('hhid','ratio')
#A=c()
#for (i in goodid.income.2011)
#  A=append(A,mdbi.inf.kanzuo.100.2011$ratio[mdbi.inf.kanzuo.100.2011$hhid==i])
#length(A)



head(mdbi.inf.kanzuo.100.2011)
head(mdbi.inf.kanzuo.100.2000)
head(mdbi.inf.kanzuo.100.1989)

describe(mdbi.inf.kanzuo.100.2011)
describe(mdbi.inf.kanzuo.100.2000)
describe(mdbi.inf.kanzuo.100.1989)

mdbi.zaikan.danqin.100<-mdbi.inf.kanzuo.100
mdbi.zaikan.danqin.100[mdbi.zaikan.danqin.100==1]=100
mdbi.zaikan.danqin.100.2011<-data.frame(mdbi.zaikan.danqin.100$newnew.wagedata.hhid ,mdbi.zaikan.danqin.100$'2011')
mdbi.zaikan.danqin.100.2000<-data.frame(mdbi.zaikan.danqin.100$newnew.wagedata.hhid ,mdbi.zaikan.danqin.100$'2000')
mdbi.zaikan.danqin.100.1989<-data.frame(mdbi.zaikan.danqin.100$newnew.wagedata.hhid ,mdbi.zaikan.danqin.100$'1989')


head(mdbi.zaikan.danqin.100.2011)
head(mdbi.zaikan.danqin.100.2000)
head(mdbi.zaikan.danqin.100.1989)

income.bili.2011<-mdbi.zaikan.danqin.100.2011
income.bili.2000<-mdbi.zaikan.danqin.100.2000
income.bili.1989<-mdbi.zaikan.danqin.100.1989

for (i in true.same.year.couple.2011)
  income.bili.2011$mdbi.zaikan.danqin.100..2011.[income.bili.2011$mdbi.zaikan.danqin.100.educ.years.hhid==i]=1
for (i in true.same.year.couple.2000)
  income.bili.2000$mdbi.zaikan.danqin.100..2000.[income.bili.2000$mdbi.zaikan.danqin.100.educ.years.hhid==i]=1
for (i in true.same.year.couple.1989)
  income.bili.1989$mdbi.zaikan.danqin.100..1989.[income.bili.1989$mdbi.zaikan.danqin.100.educ.years.hhid==i]=1

#A=c()
#for (i in goodid.income.2011)
#  A=append(A,income.bili.2011$ratio[income.bili.2011$hhid==i])
#length(A)

names(income.bili.2011)<-c('hhid','ratio')
names(income.bili.2000)<-c('hhid','ratio')
names(income.bili.1989)<-c('hhid','ratio')


length(income.bili.2011$hhid)
length(income.bili.2000$hhid)
length(income.bili.1989$hhid)


describe(income.bili.2011)
describe(goodid.income.2011)
final.2011.income.ratio<-c()
final.2011.id.yes<-c()
for (i in goodid.income.2011)
  {final.2011.income.ratio<-append(final.2011.income.ratio,income.bili.2011$ratio[income.bili.2011$hhid==i])
   final.2011.id.yes<-append(final.2011.id.yes,income.bili.2011$hhid[income.bili.2011$hhid==i])}
length(final.2011.income.ratio)
length(final.2011.id.yes)
head(final.2011.income.ratio)
describe(final.2011.income.ratio)

#for 2000
final.2000.income.ratio<-c()
final.2000.id.yes<-c()
for (i in goodid.income.2000)
  {final.2000.income.ratio<-append(final.2000.income.ratio,income.bili.2000$ratio[income.bili.2000$hhid==i])
  final.2000.id.yes<-append(final.2000.id.yes,income.bili.2000$hhid[income.bili.2000$hhid==i])}
length(final.2000.income.ratio)
length(final.2000.id.yes)
head(final.2000.income.ratio)
describe(final.2000.income.ratio)

#for 1989
final.1989.income.ratio<-c()
final.1989.id.yes<-c()
for (i in goodid.income.1989)
{final.1989.income.ratio<-append(final.1989.income.ratio,income.bili.1989$ratio[income.bili.1989$hhid==i])
 final.1989.id.yes<-append(final.1989.id.yes,income.bili.1989$hhid[income.bili.1989$hhid==i])}
length(final.1989.income.ratio)
length(final.1989.id.yes)
head(final.1989.income.ratio)
describe(final.1989.income.ratio)

#重新整理
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






id.hhpp.chuvalue.hhsize.educratio<-data.frame(thisid.for.income,final.2011.income,final.2011.chuvalue,final.2011.size,final.2011.educ.ratio)
write.csv(id.hhpp.chuvalue.hhsize.educratio,file='id-hhpp-chuvalue-hhsize-educratio.csv')
names(id.hhpp.chuvalue.hhsize.educratio)<-c('id','hhpp','chuvalue','hhsize','educratio')
huigui.try2<-lm(id.hhpp.chuvalue.hhsize.educratio$hhpp~id.hhpp.chuvalue.hhsize.educratio$chuvalue+id.hhpp.chuvalue.hhsize.educratio$hhsize+id.hhpp.chuvalue.hhsize.educratio$educratio)
summary(huigui.try2)
plot(huigui.try2)

