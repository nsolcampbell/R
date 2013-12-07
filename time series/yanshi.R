#关于路径的修改
getwd() #获取路径
setwd("D:\\dropbox\\Dropbox\\Apps\\R\\run") #修改路径

#2.R包管理
#(1)安装R包
install.packages("vcd")
#(2)载入R包
library(vcd)
#或者
require(vcd)


#3.数据基础
#(1)基本数据类型
#vector, matrix, data.frame即数组(最常用)_后面介绍



#(2)基本数据输入方法
#方法1：手动输入之1
age<-c(1,3,5,2,11,9,3,9,12,3)
weight<-c(4.4,5.3,7.2,5.2,8.5,7.3,6,10.4,10.2,6.1)
mean(weight);sd(weight);
cor(age,weight)
plot(age,weight)
#方法2：手动输入之2
#方法3：读取文件中的数据
read.table("C:\\Users\\nixujun\\Desktop\\z.txt",header=F)
#此外，还可以读取CSV格式，Excel格式


#(3)基本数据处理方法
#基本数据信息查看
a<-runif(20)
summary(a)

#数据简单处理
#1)数据之后的[]表示对数据筛选所做的条件
test<-c(1,2,3)
test[7]=5
test

#2)数据缺失的处理
test[test==NA]<-3 #这样做不行
test[is.na(test)==T]<-3
test


#但是可以这样做
test[test==3]=9
test

#提出缺失值的方法
test1<-test[is.na(test)==F]
#或者——关注感叹号!的使用
test2<-test[!is.na(test)==T]



#3)数据的类型转换
is.vector(test)
is.matrix(test)
is.data.frame(test)
as.matrix(test) #完成转换

#4.基本运算
#(1)数据集的载入
data() #查看内存中全部可用数据
data(Orange) #载入数据Orange
Orange #查看数据
summary(Orange) #查看数据的信息
#如果要看更全面的信息
library(Hmisc)
describe(Orange)
boxplot(Orange) #画出箱线图
age #但是里面的元组是无法直接查看的

#(2)线性回归
lmfit <- lm(Orange$circumference~Orange$age) #进行回归，左边的为解释变量
#简便方法？
lmfit <- with(Orange, {lm(circumference~age)})
#或者：
attach(Orange)
lmfit <- lm(circumference~age)
#不再载入用detach(Orange)


plot(Orange$age,Orange$circumference)
abline(lm(Orange$circumference~Orange$age)) #注意两个变量的先后顺序
title("Regression of age on circumference")

summary(lmfit) #用于简要分析
library(car)
outlierTest(lmfit)#用于深入分析
influence.measures(lmfit)

plot(lmfit)


#如异方差图示法检验
with(Orange, {plot(residuals(lmfit)~age,xlab="年龄",ylab="残差值",main="残差异方差图示法检验");
abline(lm(residuals(lmfit)~Orange$age))})

#还可以做各种检验
fisher.test(age,circumference)
cov(age,circumference)




#进一步做各种运算
coefficients(lmfit) #提供拟合模型的模型参数（截踞项和斜率）
confint(lmfit) #提供模拟参数的置信区间，默认为95%
fitted(lmfit) #列出拟合模型的预测值
residuals(lmfit) #列出拟合模型的残差值
anova(lmfit) #拟合模型的方差分析表
vcov(lmfit) #模型参数的协方差阵
AIC(lmfit) #赤池信息量
predict(lmfit) #用拟合模型对新的数据集预测相应变量值


#(3)多元线性回归
library(car)
data(lung)
with(lung,{lm(time~ph.karno+pat.karno)})
scatterplotMatrix(lung) #做出散点图阵，其中包含线性和平滑拟合曲线，以及相应的边际分布（核密度图和轴须图）



q() #完成退出