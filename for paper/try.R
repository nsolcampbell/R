setwd('/Users/nsol/Dropbox/手头工作/论文/DATA')
load('c12hhinc.rdata')
attach(c12hhinc)
head(c12hhinc)
#install.packages('sas7bdat') #为了读取该格式的数据
library(sas7bdat)
read.sas7bdat('m12wed.sas7bdat')

install.packages('quantreg')
library(quantreg)




library(Hmisc)
data1<-read.csv('1.csv')
data2<-read.csv('2.csv')
names(data1)<-c('perinc')
names(data2)<-c('num')
attach(data1);attach(data2)
areg<-lm(perinc~num)
summary(areg)