###循环语句

##for基本用法

#简单循环1
x<-c(1,1)
for(i in 3:30){
x[i]<-x[i-1]+x[i-2]
}
x
#简单循环2
y<-c(1)
for(n in 1:10){
y[n]<-n^2
}
y


##while基本用法
#while(condition){expr}
#当不能确定循环次数时，我们需要用while循环语句。在condition条件为真时，执行大括号内的expr语句。下面即是以while循环来计算30个Fibonacci数。
x<-c(1,1)
i<-3
while(i<=30){
x[i]<-x[i-1]+x[i-2]
i<-i+1
}
#while语句中i<=30则，默认从赋值处（上文是i<-3）开始，最后一定要有i<-i+1，否则无法完成i的赋值改变



###条件语句
##if基本用法：if (conditon) {expr1} else {expr2}
#if语句用来进行条件控制，以执行不同的语句。若condition条件为真，则执行expr1，否则执行expr2。ifesle()函数也能以简洁的方式构成条件语句。下面的一个简单的例子是要找出100以内的质数。
x <- 1:100
y <- rep(T,100)
for (i in 3:100) {
    if (all(i%%(2:(i-1))!=0)){
        y[i] <- TRUE
        } else {y[i] <- FALSE
                }
}
print(x[y])

#在上面例子里，all()函数的作用是判断一个逻辑序列是否全为真，%%的作用是返回余数。
#在if/else语句中一个容易出现的错误就是else没有放在｝的后面

#若你执行下面的示例就会出现错误。
logic = 3
x<- c(2,3)
if (logic == 2){
    y <- x^2
}
else {
  y<-x^3
}
show(y)

#这样就好了
logic = 3
x<- c(2,3)
if (logic == 2){
    y <- x^2
} else {
  y<-x^3
}
show(y)



#一个例子
#本例来自于"introduction to Scientific Programming and Simulatoin Using R"一书的习题。
#有这样一种赌博游戏，赌客首先将两个骰子随机抛掷第一次，如果点数和出现7或11，则赢得游戏，游戏结束。
#如果没有出现7或11，赌客继续抛掷，如果点数与第一次扔的点数一样，则赢得游戏，游戏结束，如果点数为7或11则输掉游戏，游戏结束。
#如果出现其它情况，则继续抛掷，直到赢或者输。用R编程来计算赌客赢的概率，以决定是否应该参加这个游戏。

craps <- function() {
    #returns TRUE if you win, FALSE otherwise
    initial.roll <- sum(sample(1:6,2,replace=T))
    if (initial.roll == 7 || initial.roll == 11) return(TRUE)
    while (TRUE) {
        current.roll <- sum(sample(1:6,2,replace=T))
        if (current.roll == 7 || current.roll == 11) {
            return(FALSE)
        } else if (current.roll == initial.roll) {
            return(TRUE)
        }
    }
}
mean(replicate(10000, craps()))
#replicate表示重复做XX次
# ||表式“或者”

##从最终结果来看，赌客赢的概率为0.46，长期来看只会往外掏钱，显然不应该参加这个游戏了。最后要说的是，本题也可以用递归来做。
