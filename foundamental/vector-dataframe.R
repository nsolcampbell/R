#回顾of vector

#which()语句可以用来找到符合条件的数据所对应的下标
#例如

x<-c(88,12,34,55)
which(x==12)


#apply函数
#很多人知道apply函数可以对矩阵进行操作，下复制一个例子，更好的启发大家的思路：

z<-matrix(c(1:6),3,2)
%     [,1] [,2]
%[1,]    1    4
%[2,]    2    5
%[3,]    3    6


f<-function(x){x/c(2,8)}

y<-apply(z,1,f)
y
#     [,1]  [,2] [,3]
#[1,]  0.5 1.000 1.50
#[2,]  0.5 0.625 0.75

t(apply(z,1,f))

#通过这个例子，我们可以自己编写一些简单的function来对矩阵操作，当然最常见的apply(z,2,mean)，
#我们的目的就是通过自编的function来替代mean做些更复杂的事情

#complete.cases()
#很多时候我们想去掉NA的情形：

d4 <- matrix(c("Jack",NA,"Jillian","John","CA","MA","MA",NA),4,2,dimnames=list(c(1:4),c("kids","states")))
#     kids states
#1    Jack     CA
#2    NA>     MA
#3 Jillian     MA
#4    John   NA>
#我们就可以用
d5<-d4[complete.cases(d4),]
d5
#     kids states
#1    Jack     CA
#3 Jillian     MA
#也就是说仅仅保留完整的条目，也许可以给大家处理实际问题带来启发



#merge()函数
#这个函数我觉得很好用，比如我有2个数据框：

d1<-matrix(c("Jack","Jill","Jillian","John","CA","MA","MA","HI"),4,2,dimnames=list(c(1:4),c("kids","states")))
#     kids states
#1    Jack     CA
#2    Jill     MA
#3 Jillian     MA
#4    John     HI
d2<-matrix(c(10,7,12,"Jill","Lillian","Jack"),3,2,dimnames=list(c(1:3),c("ages","kids")))
#  ages    kids
#1   10    Jill
#2    7 Lillian
#3   12    Jack
#然后我想通过同一个变量把2个数据框合并:
d<-merge(d1,d2)
d
#  kids states ages
#1 Jack     CA   12
#2 Jill     MA   10

d<-merge(d2,d1)
d
#挺好用的吧！

#split()函数

g<-c("M","F","F","I","M","M","F")
split(1:7,g)
#$F
#[1] 2 3 7
#$I
#[1] 4
#$M
#[1] 1 5 6
#有了这个示范，相信大家可以做很多事情了。




#同搭船, try()函数，上次在一份帖子里提过，这次再拿出来共享一下。

x<-list(10, 20, "a", "b", 30, 40)
z<-list(0)
y<-list(0)
for(i in 1:length(x)){z[[i]]<-log(x[[i]])}
z<-for (i in 1:length(x)){y[[i]]<-try(log(x[[i]]), F)}
y

 
#还有这个

for (i in 1:3) {
  print("select i")
}
for (i in 1:3) {
  print(paste("select", i))
}
#这里还是print()，容易看出来，是要把循环变量放到引号外再用paste连接，如果是要批量生成命令，有时候都不知道是这里出的错误。

 
#get()函数
#这个函数也是很好用的！看下面的循环

a=c(1,2,3)
b=c(4,5,6)
for(n in c("a","b")) print(n)
for(n in c("a","b")) print(get(n))
#相信大家能好好体会吧！如果a,b是2个文件名的时候，可以print(scan(n))

 
#pmin()函数
#比如：我们想得到
a=c(1,2,3)
b=c(2,0,4)
#中的最小，我们可以min(a,b)得到结果0，但是大家用下pmin(a,b)会得到什么呢？
pmin(a,b)



 
#order()函数
#比如我有：
d=data.frame(kids=c("a","b","c"),ages=c(2,6,4))
d
#kids ages
#1 a 2
#2 b 6
#3 c 4
#我现在想根据ages从小到大排，我们可以：

d[order(d$ages),]
#得到：
#kids ages
#1 a 2
#3 c 4
#2 b 6
#这里保存了第一列与第二列的对应关系，大家可以仔细推敲下d[order(d$ages),]为什么可以这样。



#cat()函数
#今天终于看到cat()函数的用法了：
x=c(1,2,3)
cat(x,sep=c(".","\n","\n"))
cat(x,sep=c("+","="))
cat(x,sep=c("+","=","\n"))
#看看吧!总的来说它是作为一种输出方式的函数.

#grep()函数
grep("Pole",c("Equator","North Pole","South Pole"))
#[1] 2 3
grep("pole",c("Equator","North Pole","South Pole"))
#integer(0)
#grep(pattern,x)#就是在x中找含有pattern的函数，蛮有用的


#substr()与strsplit()函数
#这两个要提下用来扫盲。。
substring("Equator",3,5)
#[1] "uat"
#strsplit("6-16-2011",split="-")
#[[1]]
#[1] "6" "16" "2011"



#regexpr()与gregexpr()函数
#这2个可能大家比较陌生点：
regexpr("uat","Equator")
#[1] 3
#就是在后面的文中找前面的，返回第几个开始
gregexpr("iss","Mississippi")
#[[1]]
#[1] 2 5
#这个与上面的类似，但找所有的



#传说中的正则表达式Regular Expressions！
#记得几个星期之前，我非常羡慕和想学一个网页抓取数据技术中的用正则表达式整理数据的方法，当时大家都说用正则表达式，
#我却不知道是什么！现在新手们的福利来了：前面介绍了grep()的用法了

grep("[au]",c("Equator","North Pole","South Pole"))
#[1] 1 3
#这里加了个[]表示找所有含a或者u!
grep("o.e",c("Equator","North Pole","South Pole"))
#[1] 2 3
#这里加了个.表示o和e之间多了随便什么字母！2个点就是2个随便什么字母，如下：
grep("N..t",c("Equator","North Pole","South Pole"))
#[1] 2
#如果我们想找.本身呢？
grep(".",c("abc","de","f.g"))
#[1] 1 2 3
#这样不行，得这样：
grep("\\.",c("abc","de","f.g"))
#[1] 3


testsuffix <- function(fn,suff) {
parts <- strsplit(fn,"\\.")
nparts <- length(parts[[1]])
return(parts[[1]][nparts] == suff)
}
testsuffix("x.y.abc","abc")
#[1] TRUE
#这里附上书中的一个例子一起学习学习！
testsuffix <- function(fn,suff) {
ncf <- nchar(fn)
dotpos <- ncf - nchar(suff) + 1
return(substr(fn,dotpos,ncf)==suff)
}
#这个也同样的功能。
#上面的几个知识点，其实都是针对string操作的，大家可以学学



#回复 第23楼 的 superdesolator：
#superdesolator，您列的这些函数都是比较常用的，非常受益。
#ps：您现在用哪本教程？
#话说这个网站对新手也非常好。http://www.mayin.org/ajayshah/KB/R/



#sprintf()函数
#大家都知道paste()函数的作用了：
paste("q",i,".pdf",sep="")
#现在我们也可以使用sprintf("q%d.pdf",i)有点像C语言！


#locator()函数！
hist(c(12,5,13,25,16))
locator(1)
#看到这个函数我瞬间哭了！！多么好的一个命令啊！
#在图上你只要点一次鼠标，它就会返回你点的位置！locator(2)可以点2个等等。。。。


#ploygon()函数
f <- function(x) return(1-exp(-x))
curve(f,0,2)
polygon(c(1.2,1.4,1.4,1.2),c(0,0,f(1.3),f(1.3)),col="gray")
#这个命令还是蛮好的，指定任意多边形的背景色或者
polygon(c(1.2,1.4,1.4,1.2),c(0,0,f(1.3),f(1.3)),density=10)用横条去填充！


#我仅代表自己感谢一下不嫌弃新手的人，能把复杂的东西简单详细讲出来的人，以后定是人品好的实力强的高手！谢谢分享！


















###关于data.frame
#对于data.frame中的子变量，可以用sapply来逐个做运算，例如
abc<-data.frame(a=1:7,b=rnorm(7),c=sample(1:10,7))
sapply(abc,mean)
sapply(abc,max)


##data.frame中的几种数据显示方法
abc<-data.frame(a=1:7,b=rnorm(7),c=sample(1:10,7))
abc$a
abc[,'a']
abc['a']
abc[['a']]
##但是上述最后一个做法不适用于多个自变量时