#最近帮朋友写了一个灰色模型GM(1,1)的R实现，参考网上现有的matlab代码，比较容易就可以弄出来。
#下面是具体过程，主函数是GM()，建立的模型是一个S3类，搭配两个自定义的泛型函数print和plot可以得到结果输出和图形。 

# 本代码用来计算GM(1,1)灰度模型
# 输入：x 原始数据，adv为外推预测步长
# 输出：actual 原始数据，fit 拟合数据,degree 拟合精度，
#       C 后验差比值，P 小概率误差，predict 外推预测值
 
 
# 主函数
GM <- function(x,adv=0) {
    x0 <- x
    k = length(x0)
    # AGO
    x1 = cumsum(x0)
    # construct matrix B & Y
    B = cbind(-0.5*(x1[-k]+x1[-1]),rep(1,times=k-1))
    Y = x0[-1]
    # compute BtB...
    BtB = t(B)%*%B
    BtB.inv = solve(BtB)
    BtY = t(B)%*%Y
 
    # estimate
    alpha = BtB.inv%*%BtY
 
    # 建立预测模型
 
    predict <- function(k) {
        y = (x0[1] - alpha[2]/alpha[1])*exp(-alpha[1]*k)+alpha[2]/alpha[1]
        return(y)
    }
    pre <- sapply(X=0:(k-1),FUN=predict)
    prediff <- c(pre[1],diff(pre))
    # 计算残差
    error <- round(abs(prediff-x0),digits=6)
    emax <- max(error)
    emin <- min(error)
    # 模型评价
    incidence <- function(x) {
         return((emin + 0.5*emax)/(x+0.5*emax))
    }
    degree <- mean(sapply(error,incidence))
 
    s1 <- sqrt(sum((x0-mean(x0))^2)/5)
    s2 <- sqrt(sum((error-mean(error))^2)/5)
 
    C <- s2/s1
 
    e <- abs(error-mean(error))
    p <- length(e<0.6745)/length(e)
 
    result <- list(actual = x0,
                   fit = prediff,
                   degree = degree,
                   C = C,
                   P = p)
 
    # 外推预测第k+adv项
    if (adv > 0) {
        pre.adv <- predict(k+adv-1)-predict(k+adv-2)
 
        result$predict <- pre.adv
     }
    class(result) <- 'GM1.1'
    return(result)
}
 
# 增加对应gm1.1类的泛型函数 print & plot
print.GM1.1 <- function(mod){
    cat('the result of GM(1,1)\n')
    cat('Actual Data:', '\n',mod$actual ,'\n')
    cat('Fit Data:', '\n',round(mod$fit,2) ,'\n')
    cat('Degree:', round(mod$degree,3) ,'\n')
    cat('C:', round(mod$C,3) ,'\n')
    cat('P:', round(mod$P,3) ,'\n')
    if (!is.null(mod$predict)) {
        cat('Predict Data:', round(mod$predict,2), '\n')
    }
}
 
plot.GM1.1 <- function(mod,adv=5){
    prex <- numeric(adv)
    x <- mod$actual
    for (k in 1:adv){
        prex[k] <- GM(x,k)$predict    
    }
 
    value = c(x,prex)
 
    res <- data.frame(index = 1:length(value),
                      value = value,
                      type = factor(c(rep(1,length(x)),rep(2,length(prex)))))
    library(ggplot2)
    p <- ggplot(res,aes(x=index,y= value))
    p + geom_point(aes(color=type),size=3)+ 
        geom_path(linetype=2) +
        theme_bw()
}
 
 
# 原始数据
x = c(26.7,31.5,32.8,34.1,35.8,37.5)
 
# 预测第7项
res <- GM(x,1)
print(res)
plot(res,3)