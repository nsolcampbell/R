final.2011.size<-c()
hhinc.2011<-hhinc[which(hhinc$wave==2011),]
for (i in goodid.income.2011)
  final.2011.size<-append(final.2011.size,hhinc.2011$hhsize[hhinc.2011$hhid==i])
length(final.2011.size)

final.2000.size<-c()
hhinc.2000<-hhinc[which(hhinc$wave==2000),]
for (i in goodid.income.2000)
  final.2000.size<-append(final.2000.size,hhinc.2000$hhsize[hhinc.2000$hhid==i])
length(final.2000.size)

final.1989.size<-c()
hhinc.1989<-hhinc[which(hhinc$wave==1989),]
for (i in goodid.income.1989)
  final.1989.size<-append(final.1989.size,hhinc.1989$hhsize[hhinc.1989$hhid==i])
length(final.1989.size)

id.hhpp.chuvalue.hhsize<-data.frame(thisid.for.income,final.2011.income,final.2011.chuvalue,final.2011.size)
write.csv(id.hhpp.chuvalue.hhsize,file='id-hhpp-chuvalue-hhsize.csv')
names(id.hhpp.chuvalue.hhsize)<-c('id','hhpp','chuvalue','hhsize')
huigui.try1<-lm(id.hhpp.chuvalue.hhsize$hhpp~id.hhpp.chuvalue.hhsize$chuvalue+id.hhpp.chuvalue.hhsize$hhsize)
summary(huigui.try1)
plot(huigui.try1)
