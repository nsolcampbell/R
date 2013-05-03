setwd("D:\\dropbox\\Dropbox\\科研工作\\L老婆毕业论文\\Data")
library(foreign)
library(Hmisc)
migind<-read.dta("migind.dta")
mighh<-read.dta("mighh.dta")
#data<-read.csv('1.csv') #会很慢

attach(migind);names(migind)
attach(mighh);names(mighh)

newmigind<-na.omit(migind)
describe(newmigind)


#hist(p103)
#migind[which.max(p103),]
#summarymigind<-summary(migind)
#write.table(summarymigind,file="summary of migind.txt")

#summarymighh<-summary(mighh)
#write.table(summarymighh,file="summary of migind.txt")



migind1<-data.frame(p102,p103,p104,p104,p104b,p107,p108,p109,p110,p111,p112,p113,p113a,p114,p115,p116,p117,p118,p119)
migind2<-data.frame(p102,p103,p104,p104,p104b,p107,p108,p109,p110,p111,p112,p113,p113a,p114,p115,p116,p117,p118,p119)


#################################################################
#对于subset的使用
x<-data.frame(matrix(1:30,nrow=5,byrow=T))
rownames(x)=c("one","two","three","four","five")
colnames(x)=c("a","b","c","d","e","f")
x
new<-subset(x,a>=14,select=a:f)
new                            ## 从a到f列选取a>14的行。
##################################################################


data<-subset(migind,is.na(migind)==F,select=1:ncol(migind))
model1 <- kmeans(newmigind,centers=6,nstart=10)
result1=cutree(model1,k=7)


##################################################################
#根据距离行层次聚类，距离使用average即类平均法距离，可以使用的其他距离有，single,complete,median,mcquitty,average,centroid,ward等
hc <- hclust(dist_tdm_removed, method = 'mcquitty') #对树进行分割
ct = cutree(hc,k=30) sort(ct) table(ct) #统计各个类的数目 #输出各个类
for(i in 1:30){print(paste("第",i,"类"));print(attr(ct[ct==i],"names"))} #输出到屏幕
for(i in 1:30){write(paste("第",i,"类"),"data.txt",append=TRUE);write(attr(ct[ct==i],"names"),"data.txt",append=TRUE)} #输出到外部文件








data=migind1 
dist.e=dist(data,method='euclidean')  #计算距离，but计算量太大
heatmap(as.matrix(dist.e),labRow = F, labCol = F)

model2=kmeans(data,centers=5,nstart=10)
result2=cutree(model2,k=6)


mds=cmdscale(dist.e,k=2,eig=T)
x = mds$points[,1]
y = mds$points[,2]
library(ggplot2)
p=ggplot(data.frame(x,y),aes(x,y))
p+geom_point(size=3,alpha=0.8,
             aes(colour=factor(model2$cluster),
               shape=iris$Species))
			   
			   
q(save = "no")





