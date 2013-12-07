load("~/Dropbox/手头工作/论文/DATA/c12hhinc.rdata")
names(c12hhinc)
attach(c12hhinc)
hist(hhsize[wave==2011],main='频率图')


par(mfrow=c(1,2))
x=hhsize[wave==2011]
x1=x[is.na(x)==F]
hist(x,
     #freq=F,
     xlab='household size in 2011',main='')
#rug(jitter(x))  #for rug plot
lines(density(x[is.na(x)==F]))
#box()


#1989
x=hhsize[wave==1989]
x2=x[is.na(x)==F]
hist(x,
     #freq=F,
     xlab='household size in 1989',main='')
#rug(jitter(x))  #for rug plot
lines(density(x[is.na(x)==F]))
#box()

#install.packages('sm')
library(sm)
sm.density.compare(x1,x2)


setwd('/Users/nsol/Dropbox/手头工作/论文/DATA')
data<-read.csv('try1.csv')

data[data==00]=0
data[data==11]=1
data[data==12]=2
data[data==13]=3
data[data==14]=4
data[data==15]=5
data[data==16]=6
data[data==21]=7
data[data==22]=8
data[data==23]=9
data[data==24]=10
data[data==25]=11
data[data==26]=12
data[data==27]=10
data[data==28]=11
data[data==29]=12
data[data==31]=13
data[data==32]=14
data[data==33]=15
data[data==34]=16
data[data==35]=17
data[data==36]=18




lines(density(hhincper[wave==2000],na.rm=T))
> plot(density(hhincper[wave==2011],na.rm=T))
> lines(density(hhincper[wave==2000],na.rm=T))



#选入非空观测做newdata
newdata<-c12hhinc[which(is.na(hhincgross)==F),]
newdata<-newdata[which(newdata$hhincgross>0),]
newdata<-newdata[which(is.na(newdata$hhsize)==F),]
head(newdata)
aa<-newdata$hhincgross/newdata$hhsize

aaa<-factor(wave,levels=c(1989,1991,1993,1997,2000,2004,2006,2009,2011),labels=c('1989','1991','1993
                                                                               ','1997','2000','2004'
                                                                               ,'2006','2009','2011'))
aa<-aa[aa>0]
sm.density.compare(aa,newdata$wave,xlab='Income Per Person')
colfill<-c(2:(1+length(levels(aaa))))
legend(-1, 1.9, c("sin", "cos", "tan"), col = c(3, 4, 6),
       text.col = "green4", lty = c(2, -1, 1), pch = c(NA, 3, 4),
       merge = TRUE, bg = "gray90")
legend(locator(1), levels(aaa), fill=colfill)