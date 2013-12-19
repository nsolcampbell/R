par(mfrow=c(1,3))
plot(density(educdata$age[educdata$wave==2011],na.rm=T),main='',xlab='Wave=2011')
plot(density(educdata$age[educdata$wave==2000],na.rm=T),main='',xlab='Wave=2000')
plot(density(educdata$age[educdata$wave==1989],na.rm=T),main='',xlab='Wave=1989')

data.1989<-data.frame(data.1989.income,final.1989.id.yes,data.1989.chuvalue,data.1989.size,data.1989.educ.ratio,data.1989.income.ratio)
names(data.1989)<-c('income','hhid','agestructure','hhsize','educratio','wageratio')
head(data.1989)


par(mfrow=c(1,1))
#install.packages('scatterplot3d')
library(scatterplot3d)
s3d<-scatterplot3d(data.1989$income,data.1989$agestructure,data.1989$hhsize,
                   pch=16,
                   highlight.3d=TRUE,
                   type='h',
                   main='3D Scatter Plot with Vertical Lines and Regression Plane')
#,data.1989$wageratio
#fit<-lm(data.1989$income~data.1989$agestructure+data.1989$wageratio)
#s3d$plane3d(fit)


#可旋转的
install.packages("rgl")
library(rgl)
with(data.1989,plot3d(income,hhsize,agestructur))