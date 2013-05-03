
library(MASS)
library(cluster)
.search.vanilla <- search()
detach.everything <- function () {
  for (i in setdiff( search(), .search.vanilla )) {
    detach(pos = match(i, search()))
  }
}
detach.everything()

png(file="g1.png", width=600, height=600)
  library(Zelig)
  d <- data.frame(
    y = crabs$sp,
    x1 = crabs$FL,
    x2 = crabs$RW
  )
  r <- zelig( y ~ x1 + x2, model="probit", data=d )
  summary(r)
  op <- par(mfrow=c(2,2), mar=c(4,4,3,2))
  plot(r)
  par(op)
dev.off()

png(file="g2.png", width=600, height=600)
  x <- rnorm(100)
  hist(x, col = "light blue")
dev.off()

png(file="g3.png", width=600, height=600)
  N <- 100
  x <- rnorm(N)
  y <- x + rnorm(N)
  plot(y ~ x)
dev.off()

png(file="g4.png", width=600, height=600)
  N <- 100
  x <- rnorm(N)
  y <- x + rnorm(N)
  plot(y ~ x)
  abline( lm(y ~ x), col = "red" )
dev.off()
detach.everything()

png(file="g5.png", width=600, height=600)
  data(EuStockMarkets)
  x <- EuStockMarkets
  # We aren't interested in the spot prices, but in the returns
  # return[i] = ( price[i] - price[i-1] ) / price[i-1]
  y <- apply(x, 2, function (x) { diff(x)/x[-length(x)] })
  # We normalize the data
  z <- apply(y, 2, function (x) { (x-mean(x))/sd(x) })
  # A single time series
  r <- z[,1]
  # The runs
  f <- factor(cumsum(abs(diff(sign(r))))/2)
  r <- r[-1]
  accumulated.return <- tapply(r, f, sum)
  trend.number <- table(f)
  boxplot(abs(accumulated.return) ~ trend.number, col='pink',
          main="Accumulated return")
dev.off()

png(file="g6.png", width=600, height=600)
  boxplot(abs(accumulated.return)/trend.number ~ trend.number,
          col='pink', main="Average return")
dev.off()

png(file="g7.png", width=600, height=600)
  op <- par(mfrow=c(2,2))
  for (i in 1:4) {
    r <- z[,i]
    f <- factor(cumsum(abs(diff(sign(r))))/2)
    r <- r[-1]
    accumulated.return <- tapply(r, f, sum)
    trend.number <- table(f)
    boxplot(abs(accumulated.return) ~ trend.number, col='pink')
  }
  par(op)    
dev.off()

png(file="g8.png", width=600, height=600)
  op <- par(mfrow=c(2,2))
  for (i in 1:4) {
    r <- z[,i]
    f <- factor(cumsum(abs(diff(sign(r))))/2)
    r <- r[-1]
    accumulated.return <- tapply(r, f, sum)
    trend.number <- table(f)
    boxplot(abs(accumulated.return)/trend.number ~ trend.number, col='pink')
  }
  par(op)    
dev.off()

png(file="g9.png", width=600, height=600)
  data(volcano)
  M <- volcano
  n <- dim(M)[1]
  m <- dim(M)[2]
  M1 <- M [1:(n-1),] [,1:(m-1)]
  M2 <- M [2:n,] [,1:(m-1)]
  M3 <- M [1:(n-1),] [,2:m]
  M4 <- M [2:n,] [,2:m]
  # Overlapping quadripixels
  M0 <- (M1+M2+M3+M4)/4

  # Non-overlapping quadripixels
  nn <- floor((n-1)/2)
  mm <- floor((m-1)/2)
  M00 <- M0 [2*(1:nn),] [,2*(1:mm)]

  op <- par(mfrow=c(2,2))
  image(M, main="Initial image")
  image(M0, main="Overlapping Quadripixels")
  image(M00, main="Non Overlapping Quadripixels")
  par(op)
dev.off()

png(file="g10.png", width=600, height=600)
  n <- 100
  m <- matrix(runif(2*n),nc=2)
  library(ape)
  r <- mst(dist(m)) # The incidence matrix (of the minimum spanning
                    # tree of the points)
  plot(m)
  n <- dim(r)[1]
  w <- which(r!=0)
  i <- as.vector(row(r))[w]
  j <- as.vector(col(r))[w]
  segments( m[i,1], m[i,2], m[j,1], m[j,2], col='red' )
dev.off()

png(file="g11.png", width=600, height=600)
  several.times <- function (n, f, ...) {
    for (i in 1:n) {
      f(...)
    }
  }
  matrix.multiplication <- function (s) {
    A <- matrix(1:(s*s), nr=s, nc=s) 
    B <- matrix(1:(s*s), nr=s, nc=s) 
    C <- A %*% B
  }
  v <- NULL
  for (i in 2:10) {
    v <- append(
      v, 
      system.time( 
        several.times( 
          10000, 
          matrix.multiplication, 
          i 
        )
      ) [1]
    )
  }
  plot(v, type = 'b', pch = 15, 
       main = "Matrix product computation time")
dev.off()

png(file="g12.png", width=600, height=600)
  x <- seq(0,4, length=100)
  y <- sqrt(x)
  plot(y~x, type='l', lwd=3, main=expression(y == sqrt(x)) )
dev.off()
detach.everything()

png(file="g13.png", width=600, height=600)
  n <- 1000
  k <- 20
  p <- 3
  i <- sample(1:p, n, replace=TRUE)
  x <- 10 * matrix(rnorm(p*k), nr=p, nc=k)
  x <- x[i,] + matrix(rnorm(n*k), nr=n, nc=k)
  L1L2 <- function (x) {
    cbind(L1 = apply(x, 1, mean),
          L2 = apply(x, 1, sd))
  }
  plot(L1L2(x), col=i)
dev.off()

png(file="g14.png", width=600, height=600)
  x <- crabs$FL
  y <- crabs$CL  # The two vectors need not 
                 # have the same length
  op <- par(mfrow=c(2,1))
  hist(x, col="light blue", xlim=c(0,50))
  hist(y, col="light blue", xlim=c(0,50))
  par(op)
dev.off()

png(file="g15.png", width=600, height=600)
  op <- par(mfrow=c(2,1))
  hist( (x - mean(x)) / sd(x), 
        col = "light blue",
        xlim = c(-3, 3) )
  hist( (y - mean(y)) / sd(y), 
        col = "light blue",
        xlim = c(-3, 3) )
  par(op)
dev.off()

png(file="g16.png", width=600, height=600)
  N <- 50           # Sample size
  set.seed(2)
  x1 <- runif(N)    # Uniform distribution
  x2 <- rt(N,2)     # Fat-tailed distribution
  x3 <- rexp(N)     # Skewed distribution
  x4 <- c(x2,20)    # Outlier (not that uncommon,
                    # with fat-tailed distributions)
  f <- function (x, ...) {
    x <- (x - mean(x)) / sd(x)
    N <- length(x)
    hist( x,
          col = "light blue",
          xlim = c(-3, 3),
          ylim = c(0, .8),
          probability = TRUE,
          ...
        )
    lines(density(x), 
          col = "red", lwd = 3)
    rug(x)
  }
  op <- par(mfrow=c(2,2))
  f(x1, main = "Uniform distribution")
  f(x2, main = "Fat-tailed distribution")
  f(x3, main = "Skewed distribution")
  f(x4, main = "As above, with one outlier")  
  par(op)
dev.off()

png(file="g17.png", width=600, height=600)
  x <- read.csv("2006-08-27_pe.csv")
  op <- par(mfrow=c(1,2))
  plot(p ~ eps, data=x, main="Before")
  plot(log(p) ~ log(eps), data=x, main="After")
  par(op)
dev.off()

png(file="g18.png", width=600, height=600)
  f <- function (x, main, FUN) { 
    hist(x, 
         col = "light blue", 
         probability = TRUE, 
         main = paste(main, "(before)"),
         xlab = "")
    lines(density(x), col = "red", lwd = 3)
    rug(x)
    x <- FUN(x)
    hist(x, 
         col = "light blue", 
         probability = TRUE, 
         main = paste(main, "(after)"),
         xlab = "")
    lines(density(x), col = "red", lwd = 3)
    rug(x)
  }
  op <- par(mfrow=c(2,2))
  f(x3, 
    main="Skewed distribution", 
    FUN = log)
  f(x2, 
    main="Fat tailed distribution", 
    FUN = function (x) {  # If you have an idea of the 
                          # distribution followed by 
                          # your variable, you can use 
                          # that distribution to get a 
                          # p-value (i.e., a number between
                          # 0 and 1: just apply the inverse
                          # of the cumulative distribution 
                          # function -- in R, it is called
                          # the p-function of the 
                          # distribution) then apply the 
                          # gaussian cumulative distribution 
                          # function (in R, it is called the 
                          # quantile function or the 
                          # q-function).
            qnorm(pcauchy(x)) 
          } 
    )
  par(op)
dev.off()

png(file="g19.png", width=600, height=800)
  uniformize <- function (x) { # This could be called 
                               # "forceful uniformization".
                               # More about it when we introduce
                               # the notion of copula.
    x <- rank(x, 
              na.last = "keep", 
              ties.method = "average")
    n <- sum(!is.na(x))
    x / (n + 1)
  }
  normalize <- function (x) {
    qnorm(uniformize(x))
  }
  op <- par(mfrow=c(4,2))
  f(x1, FUN = normalize, main = "Uniform distribution")
  f(x3, FUN = normalize, main = "Skewed distribution")
  f(x2, FUN = normalize, main = "Fat-tailed distribution")
  f(x4, FUN = normalize, main = "Idem with one outlier")
  par(op)
dev.off()

png(file="g20.png", width=600, height=600)
  library(e1071) # For the "skewness" and "kurtosis" functions
  n <- 1000
  x <- rnorm(n)
  op <- par(mar=c(3,3,4,2)+.1)
  hist(x, col="light blue", probability=TRUE, 
       main=paste("skewness =", round(skewness(x), digits=2)),
       xlab="", ylab="")
  lines(density(x), col="red", lwd=3)
  par(op)
dev.off()

png(file="g21.png", width=600, height=600)
  x <- rexp(n)
  op <- par(mar=c(3,3,4,2)+.1)
  hist(x, col="light blue", probability=TRUE, 
       main=paste("skewness =", round(skewness(x), digits=2)),
       xlab="", ylab="")
  lines(density(x), col="red", lwd=3)
  par(op)
dev.off()

png(file="g22.png", width=600, height=600)
  x <- -rexp(n)
  op <- par(mar=c(3,3,4,2)+.1)
  hist(x, col="light blue", probability=TRUE, 
       main=paste("skewness =", round(skewness(x), digits=2)),
       xlab="", ylab="")
  lines(density(x), col="red", lwd=3)
  par(op)
dev.off()

png(file="g23.png", width=600, height=600)
  library(e1071) # For the "skewness" and "kurtosis" functions
  n <- 1000
  x <- rnorm(n)
  qqnorm(x, main=paste("kurtosis =", round(kurtosis(x), digits=2),
                       "(gaussian)"))
  qqline(x, col="red")
  op <- par(fig=c(.02,.5,.5,.98), new=TRUE)
  hist(x, probability=T,
       col="light blue", xlab="", ylab="", main="", axes=F)
  lines(density(x), col="red", lwd=2)
  box()
  par(op)
dev.off()

png(file="g24.png", width=600, height=600)
  set.seed(1)
  x <- rt(n, df=4)
  qqnorm(x, main=paste("kurtosis =", round(kurtosis(x), digits=2),
                       "(T, df=4)"))
  qqline(x, col="red")
  op <- par(fig=c(.02,.5,.5,.98), new=TRUE)
  hist(x, probability=T,
       col="light blue", xlab="", ylab="", main="", axes=F)
  lines(density(x), col="red", lwd=2)
  box()
  par(op)
dev.off()

png(file="g25.png", width=600, height=600)
  x <- runif(n)
  qqnorm(x, main=paste("kurtosis =", round(kurtosis(x), digits=2),
                       "(uniform)"))
  qqline(x, col="red")
  op <- par(fig=c(.02,.5,.5,.98), new=TRUE)
  hist(x, probability=T,
       col="light blue", xlab="", ylab="", main="", axes=F)
  lines(density(x), col="red", lwd=2)
  box()
  par(op)
dev.off()

png(file="g26.png", width=600, height=600)
  op <- par(mfrow=c(2,2), mar=c(3,2,2,2)+.1)
  data(EuStockMarkets)
  x <- EuStockMarkets
  # We aren't interested in the spot prices, but in the returns
  # return[i] = ( price[i] - price[i-1] ) / price[i-1]
  y <- apply(x, 2, function (x) { diff(x)/x[-length(x)] })
  # We normalize the data
  z <- apply(y, 2, function (x) { (x-mean(x))/sd(x) })
  for (i in 1:4) {
    d <- density(z[,i])
    plot(d$x,log(d$y),ylim=c(-5,1),xlim=c(-5,5))
    curve(log(dnorm(x)),col='red',add=T)
    mtext(colnames(x)[i], line=-1.5, font=2)
  }
  par(op)
  mtext("Are stock returns gaussian?", line=3, font=2)
dev.off()

png(file="g27.png", width=600, height=600)
  n <- dim(z)[1]
  N <- 2000    # Two thousand samples of the same size
  m <- matrix(rnorm(n*N), nc=N, nr=n)
  a <- apply(m^3,2,mean)
  b <- apply(z^3,2,mean)
  op <- par(mar=c(3,3,4,1)+.1)
  hist(a, col='light blue', xlim=range(c(a,b)),
       main="Third moment (skewness)",
       xlab="", ylab="")
  h <- rep(.2*par("usr")[3] + .8*par("usr")[4], length(b))
  points(b, h, type='h', col='red',lwd=3)
  points(b, h, col='red', lwd=3)
  text(b, h, names(b), pos=3)
  par(op)
dev.off()

png(file="g28.png", width=600, height=600)
  n <- dim(z)[1]
  N <- 2000
  m <- matrix(rnorm(n*N), nc=N, nr=n)
  a <- apply(m^4,2,mean) - 3
  b <- apply(z^4,2,mean) - 3
  op <- par(mar=c(3,3,4,1)+.1)
  hist(a, col='light blue', xlim=range(c(a,b)),
       main="Expected kurtosis distribution and observed values",
       xlab="", ylab="")
  h <- rep(.2*par("usr")[3] + .8*par("usr")[4], length(b))
  points(b, h, type='h', col='red',lwd=3)
  points(b, h, col='red', lwd=3)
  text(b, h, names(b), pos=3)
  par(op)
dev.off()

png(file="g29.png", width=600, height=600)
  data(EuStockMarkets)
  x <- EuStockMarkets
  y <- apply(x, 2, function (x) { diff(x)/x[-length(x)] })

  library(lmomco)
  n <- dim(z)[1]
  N <- 200
  m <- matrix(rnorm(n*N), nc=N, nr=n)

  # We normalize the data in the same way
  f <- function (x) { 
    r <- lmom.ub(x)
    (x - r$L1) / r$L2
  }
  z <- apply(y, 2, f)
  m <- apply(m, 2, f)

  a <- apply(m, 2, function (x) lmom.ub(x)$TAU3)
  b <- apply(z, 2, function (x) lmom.ub(x)$TAU3)
  op <- par(mar=c(3,3,4,1)+.1)
  hist(a, col='light blue', xlim=range(c(a,b)),
       main="Expected L-skewness distribution and observed values",
       xlab="", ylab="")
  h <- rep(.2*par("usr")[3] + .8*par("usr")[4], length(b))
  points(b, h, type='h', col='red',lwd=3)
  points(b, h, col='red', lwd=3)
  text(b, h, names(b), pos=3)
  par(op)
dev.off()

png(file="g30.png", width=600, height=600)
  a <- apply(m, 2, function (x) lmom.ub(x)$TAU4)
  b <- apply(z, 2, function (x) lmom.ub(x)$TAU4)
  op <- par(mar=c(3,3,4,1)+.1)
  hist(a, col='light blue', xlim=range(c(a,b)),
       main="Expected L-kurtosis distribution and observed values",
       xlab="", ylab="")
  h <- rep(.2*par("usr")[3] + .8*par("usr")[4], length(b))
  points(b, h, type='h', col='red',lwd=3)
  points(b, h, col='red', lwd=3)
  text(b, h, names(b), pos=3)
  par(op)
dev.off()

png(file="g31.png", width=600, height=200)
  data(faithful)
  stripchart(faithful$eruptions, main="The \"stripchart\" function")
dev.off()

png(file="g32.png", width=600, height=200)
  # Only horizontal noise
  stripchart(faithful$eruptions, jitter=TRUE,
             main="jittered scatterplot")
dev.off()

png(file="g33.png", width=600, height=200)
  stripchart(faithful$eruptions, method='jitter',
             main="jittered scatterplot")
dev.off()

png(file="g34.png", width=600, height=200)
  my.jittered.stripchart <- function (x) {
    x.name <- deparse(substitute(x))
    n <- length(x)
    plot( runif(n) ~ x, xlab=x.name, ylab='noise',
          main="jittered scatterplot" )
  }
  my.jittered.stripchart(faithful$eruptions)
dev.off()

png(file="g35.png", width=600, height=200)
  my.jittered.stripchart <- function (x) {
    x.name <- deparse(substitute(x))
    n <- length(x)
    x <- x + diff(range(x))*.05* (-.5+runif(n))
    plot( runif(n) ~ x, 
          xlab=paste("jittered", x.name), ylab='noise',
          main="jittered scatterplot" )
  }
  my.jittered.stripchart(faithful$eruptions)
dev.off()

png(file="g36.png", width=600, height=600)
  op <- par(mar=c(3,4,2,2)+.1)
  plot( sort( faithful$eruptions ),
        xlab = "" 
      )
  par(op)
dev.off()

png(file="g37.png", width=600, height=600)
  op <- par(mar=c(3,4,2,2)+.1)
  plot(sort(faithful$eruptions), xlab="")
  rug(faithful$eruptions, side=2)
  par(op)
dev.off()

png(file="g38.png", width=600, height=600)
  op <- par(mar=c(3,4,2,2)+.1)
  x <- round( rnorm(100), digits=1 )
  plot(sort(x))
  rug(jitter(x), side=2)
  par(op)
dev.off()

png(file="g39.png", width=600, height=600)
  cumulated.frequencies <- function (x, main="") {
    x.name <- deparse(substitute(x))
    n <- length(x)
    plot( 1:n ~ sort(x), 
          xlab = x.name, 
          ylab = 'Cumulated frequencies',
          main = main
        )
  }    
  cumulated.frequencies(faithful$eruptions,
                        main = "Eruption lengths")
dev.off()

png(file="g40.png", width=600, height=800)
  data(islands)
  dotchart(islands, main="Island area")
dev.off()

png(file="g41.png", width=600, height=800)
  dotchart(sort(log(islands)), 
           main="Island area (logarithmic scale)")
dev.off()

png(file="g42.png", width=800, height=600)
  op <- par(mfcol=c(2,4), mar=c(2,2,1,1)+.1)
  do.it <- function (x) {
    hist(x, probability=T, col='light blue',
         xlab="", ylab="", main="", axes=F)
    axis(1)
    lines(density(x), col='red', lwd=3)
    x <- sort(x)
    q <- ppoints(length(x))
    plot(q~x, type='l',
         xlab="", ylab="", main="")
    abline(h=c(.25,.5,.75), lty=3, lwd=3, col='blue')
  }
  n <- 200
  do.it(rnorm(n))
  do.it(rlnorm(n))
  do.it(-rlnorm(n))
  do.it(rnorm(n, c(-5,5)))
  par(op)
dev.off()

png(file="g43.png", width=600, height=600)
  N <- 2000
  x <- rnorm(N)
  op <- par(mar=c(0,0,0,0), oma=c(0,0,0,0)+.1)
  layout(matrix(c(1,1,1,2), nc=1))
  y <- ppoints( length(x) )
  plot(sort(x), y, type="l", lwd=3,
       xlab="", ylab="", main="")
  abline(h=c(0,.25,.5,.75,1), lty=3)
  abline(v = quantile(x), col = "blue", lwd = 3, lty=2)
  points(quantile(x), c(0,.25,.5,.75,1), lwd=10, col="blue")
  boxplot(x, horizontal = TRUE, col = "pink", lwd=5)  
  abline(v = quantile(x), col = "blue", lwd = 3, lty=2)
  par(new=T)
  boxplot(x, horizontal = TRUE, col = "pink", lwd=5)  
  par(op)
dev.off()

png(file="g44.png", width=600, height=600)
  boxplot(faithful$eruptions, range=0)
dev.off()

png(file="g45.png", width=600, height=200)
  boxplot(faithful$eruptions, range=0, horizontal=TRUE)
dev.off()

png(file="g46.png", width=600, height=600)
  op <- par(mfrow=c(1,2), mar=c(3,2,4,2)+.1)
  do.it <- function (x, xlab="", ylab="", main="") {
    d <- density(x)
    plot(d, type='l', xlab=xlab, ylab=ylab, main=main)
    q <- quantile(x)
    do.it <- function (i, col) {
      x <- d$x[i]
      y <- d$y[i]
      polygon( c(x,rev(x)), c(rep(0,length(x)),rev(y)), border=NA, col=col )
    }
    do.it(d$x <= q[2], 'red')
    do.it(q[2] <= d$x & d$x <= q[3], 'green')
    do.it(q[3] <= d$x & d$x <= q[4], 'blue')
    do.it(d$x >= q[4], 'yellow')
    lines(d, lwd=3)
  }
  do.it( rnorm(2000), main="Gaussian" )
  do.it( rexp(200), main="Exponential" )
  par(op)
  mtext("Quartiles", side=3, line=3, font=2, cex=1.2)
dev.off()

png(file="g47.png", width=600, height=200)
  boxplot(faithful$eruptions, horizontal = TRUE,
          main = "No outliers")
dev.off()

png(file="g48.png", width=600, height=200)
  # There are outliers, they might bring trouble, 
  # but it is normal, it is not pathological
  boxplot(rnorm(500), horizontal = TRUE,
          main = "Normal outliers")
dev.off()

png(file="g49.png", width=600, height=200)
  x <- c(rnorm(30),20)
  x <- sample(x, length(x))
  boxplot( x, horizontal = TRUE,
           main = "An outlier" )
dev.off()

png(file="g50.png", width=600, height=200)
  library(boot)
  data(aml)
  boxplot( aml$time, horizontal = TRUE,
           main = "An outlier" )
dev.off()

png(file="g51.png", width=600, height=200)
  data(attenu)
  boxplot(attenu$dist, horizontal = TRUE,
          main = "Non gaussian (asymetric) data")
dev.off()

png(file="g52.png", width=600, height=200)
  data(attenu)
  boxplot(log(attenu$dist), horizontal = TRUE,
          main = "Transformed variable")
dev.off()

png(file="g53.png", width=600, height=300)
  y <- c(rnorm(10+100+1000+10000+100000))
  x <- c(rep(1,10), rep(2,100), rep(3,1000), rep(4,10000), rep(5,100000))
  x <- factor(x)
  plot(y~x, 
       horizontal = TRUE, 
       col = "pink",
       las = 1,
       xlab = "", ylab = "",
       main = "The larger the sample, the more outliers")
dev.off()

png(file="g54.png", width=600, height=200)
  boxplot(faithful$eruptions, 
          notch = TRUE,
          horizontal = TRUE,
          main = "Confidence interval on the median...")
dev.off()

png(file="g55.png", width=600, height=300)
  library(boot)
  data(breslow)
  boxplot(breslow$n, 
          notch = TRUE,
          horizontal = TRUE, 
          col = "pink",
          main = "...that goes beyond the quartiles")
dev.off()

png(file="g56.png", width=600, height=200)
  boxplot(faithful$eruptions, 
          horizontal = TRUE,
          col = "pink")
  rug(faithful$eruption, 
      ticksize = .2)
dev.off()

png(file="g57.png", width=600, height=600)
  hist(faithful$eruptions)
dev.off()

png(file="g58.png", width=600, height=600)
  hist(faithful$eruptions, breaks=20, col="light blue")
dev.off()

png(file="g59.png", width=600, height=600)
  op <- par(mfrow=c(2,1), mar=c(2,2,2,1)+.1)
  hist(faithful$eruptions, breaks=seq(1,6,.5), 
       col='light blue',
       xlab="", ylab="", main="")
  hist(faithful$eruptions, breaks=.25+seq(1,6,.5), 
       col='light blue',
       xlab="", ylab="", main="")
  par(op)
  mtext("Is the first peak symetric or not?", 
        side=3, line=2.5, font=2.5, size=1.5)
dev.off()

png(file="g60.png", width=600, height=600)
  hist(faithful$eruptions, 
       probability=TRUE, breaks=20, col="light blue",
       xlab="", ylab="",
       main="Histogram and density estimation")
  points(density(faithful$eruptions, bw=.1), type='l', 
         col='red', lwd=3)
dev.off()

png(file="g61.png", width=600, height=600)
  hist(faithful$eruptions, 
       probability=TRUE, breaks=20, col="light blue",
       xlab="", ylab="",
       main="Histogram and density estimation")
  points(density(faithful$eruptions, bw=1),  type='l', 
         lwd=3, col='black')
  points(density(faithful$eruptions, bw=.5), type='l', 
         lwd=3, col='blue')
  points(density(faithful$eruptions, bw=.3), type='l', 
         lwd=3, col='green')
  points(density(faithful$eruptions, bw=.1), type='l', 
         lwd=3, col='red')
dev.off()

png(file="g62.png", width=600, height=600)
  hist(faithful$eruptions, 
       probability=TRUE, breaks=20, col="light blue",
       main="")
  rug(faithful$eruptions)
  points(density(faithful$eruptions, bw=.1), type='l', lwd=3, col='red')
  f <- function(x) {
    dnorm(x, 
          mean=mean(faithful$eruptions), 
          sd=sd(faithful$eruptions),
    ) 
  }
  curve(f, add=T, col="red", lwd=3, lty=2)
dev.off()

png(file="g63.png", width=600, height=600)
  symetry.plot <- function (x0, 
                            main="Symetry plot",
                            breaks="Sturges", ...) {
    x <- x0[ !is.na(x0) ]
    x <- sort(x)
    x <- abs(x - median(x))
    n <- length(x)
    nn <- ceiling(n/2)
    plot( x[n:(n-nn+1)] ~ x[1:nn] ,
          xlab='Distance below median',
          ylab='Distance above median', 
          main=main,
          ...)
    abline(0,1, col="blue", lwd=3)  
    op <- par(fig=c(.02,.5,.5,.98), new=TRUE)
    hist(x0, probability=T, breaks=breaks,
         col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x0), col="red", lwd=2)
    box()
    par(op)
  }

  symetry.plot(rnorm(500), 
               main="Symetry plot (gaussian distribution)")
dev.off()

png(file="g64.png", width=600, height=600)
  symetry.plot(rexp(500),
               main="Symetry plot (exponential distribution)")

dev.off()

png(file="g65.png", width=600, height=600)
  symetry.plot(-rexp(500),
               main="Symetry plot (negative skewness)")

dev.off()

png(file="g66.png", width=600, height=600)
  symetry.plot(rexp(500),
               main="Symetry plot, logarithmic scales)")

dev.off()

png(file="g67.png", width=600, height=600)
  symetry.plot(faithful$eruptions, breaks=20)
dev.off()

png(file="g68.png", width=600, height=600)
  symetry.plot.2 <- function (x, N=1000, 
                              pch=".", cex=1, ...) {
    x <- x[ !is.na(x) ]
    x <- sort(x)
    x <- abs(x - median(x))
    n <- length(x)
    nn <- ceiling(n/2)
    plot( x[n:(n-nn+1)] ~ x[1:nn] ,
          xlab='Distance below median',
          ylab='Distance above median', 
          ...)
    for (i in 1:N) {
      y <- sort( rnorm(n) )
      y <- abs(y - median(y))
      m <- ceiling(n/2)
      points( y[n:(n-m+1)] ~ y[1:m], 
              pch=pch, cex=cex, col='red' )
    }
    points(x[n:(n-nn+1)] ~ x[1:nn] , ...)
    abline(0,1, col="blue", lwd=3)  
  }

  n <- 100
  symetry.plot.2( rnorm(n), pch='.', lwd=3, 
                  main=paste("Symetry plot: gaussian,", n, "observations"))
dev.off()

png(file="g69.png", width=600, height=600)
  n <- 10
  symetry.plot.2( rnorm(n), pch=15, lwd=3, type="b", cex=.5,
                  main=paste("Symetry plot: gaussian,", n, "observations"))
dev.off()

png(file="g70.png", width=600, height=600)
  robust.symetry.plot <- function (x, 
                                   N = max(ceiling(1000/length(x)),2),
                                   alpha = .05, 
                                   xlab = "Distance below the median",
                                   ylab = "Distance above", 
                                   main = "Symetry plot",
                                   ...) {
    cat(N, "\n")
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)], 
          xlab = xlab, ylab = ylab, 
          main = main,
          ... )
    # The symetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn) 
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...) 
    # The 5% confidence interval stemming from the resampled data
    require(quantreg)
    for (tau in c(alpha, 1-alpha)) {
      r <- lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2)
    # The histogram, in a corner
    op <- par(fig = if (skewness(x)>0) 
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
  robust.symetry.plot(EuStockMarkets[,"CAC"])
dev.off()

png(file="g71.png", width=600, height=600)
  robust.symetry.plot <- function (x, 
                                   N = max(ceiling(1000/length(x)),2),
                                   alpha = .05, 
                                   xlab = "Distance below the median",
                                   ylab = "Distance above", 
                                   main = "Symetry plot",
                                   ...) {
    cat(N, "\n")
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)], 
          xlab = xlab, ylab = ylab, 
          main = main,
          ... )
    # The symetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn) 
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...) 
    # The 5% confidence interval stemming from the resampled data
    require(quantreg)
    for (tau in c(alpha, 1-alpha)) {
      r <- lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2)
    # The histogram, in a corner
    op <- par(fig = if (skewness(x)>0) 
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
  robust.symetry.plot(EuStockMarkets[,"CAC"])
dev.off()

png(file="g72.png", width=600, height=600)
  robust.symetry.plot <- function (x, 
                                   N = max(ceiling(1000/length(x)),2),
                                   alpha = .05, 
                                   xlab = "Distance below the median",
                                   ylab = "Distance above", 
                                   main = "Symetry plot",
                                   ...) {
    cat(N, "\n")
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)], 
          xlab = xlab, ylab = ylab, 
          main = main,
          ... )
    # The symetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn) 
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...) 
    # The 5% confidence interval stemming from the resampled data
    require(quantreg)
    for (tau in c(alpha, 1-alpha)) {
      r <- lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2)
    # The histogram, in a corner
    op <- par(fig = if (skewness(x)>0) 
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
  robust.symetry.plot(EuStockMarkets[,"CAC"])
dev.off()

png(file="g73.png", width=600, height=600)
  robust.symetry.plot <- function (x, 
                                   N = max(ceiling(1000/length(x)),2),
                                   alpha = .05, 
                                   xlab = "Distance below the median",
                                   ylab = "Distance above", 
                                   main = "Symetry plot",
                                   ...) {
    cat(N, "\n")
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)], 
          xlab = xlab, ylab = ylab, 
          main = main,
          ... )
    # The symetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn) 
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...) 
    # The 5% confidence interval stemming from the resampled data
    require(quantreg)
    for (tau in c(alpha, 1-alpha)) {
      r <- lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2)
    # The histogram, in a corner
    op <- par(fig = if (skewness(x)>0) 
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
  robust.symetry.plot(EuStockMarkets[,"CAC"])
dev.off()

png(file="g74.png", width=600, height=600)
  robust.symetry.plot <- function (x, 
                                   N = max(ceiling(1000/length(x)),2),
                                   alpha = .05, 
                                   xlab = "Distance below the median",
                                   ylab = "Distance above", 
                                   main = "Symetry plot",
                                   ...) {
    cat(N, "\n")
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)], 
          xlab = xlab, ylab = ylab, 
          main = main,
          ... )
    # The symetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn) 
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...) 
    # The 5% confidence interval stemming from the resampled data
    require(quantreg)
    for (tau in c(alpha, 1-alpha)) {
      r <- lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2)
    # The histogram, in a corner
    op <- par(fig = if (skewness(x)>0) 
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
  robust.symetry.plot(EuStockMarkets[,"CAC"])
dev.off()

png(file="g75.png", width=600, height=600)
  robust.symetry.plot <- function (x, 
                                   N = max(ceiling(1000/length(x)),2),
                                   alpha = .05, 
                                   xlab = "Distance below the median",
                                   ylab = "Distance above", 
                                   main = "Symetry plot",
                                   ...) {
    cat(N, "\n")
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)], 
          xlab = xlab, ylab = ylab, 
          main = main,
          ... )
    # The symetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn) 
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...) 
    # The 5% confidence interval stemming from the resampled data
    require(quantreg)
    for (tau in c(alpha, 1-alpha)) {
      r <- lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2)
    # The histogram, in a corner
    op <- par(fig = if (skewness(x)>0) 
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
  robust.symetry.plot(EuStockMarkets[,"CAC"])
dev.off()

png(file="g76.png", width=600, height=600)
  robust.symetry.plot <- function (x, 
                                   N = max(ceiling(1000/length(x)),2),
                                   alpha = .05, 
                                   xlab = "Distance below the median",
                                   ylab = "Distance above", 
                                   main = "Symetry plot",
                                   ...) {
    cat(N, "\n")
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)], 
          xlab = xlab, ylab = ylab, 
          main = main,
          ... )
    # The symetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn) 
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...) 
    # The 5% confidence interval stemming from the resampled data
    require(quantreg)
    for (tau in c(alpha, 1-alpha)) {
      r <- lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2)
    # The histogram, in a corner
    op <- par(fig = if (skewness(x)>0) 
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
  robust.symetry.plot(EuStockMarkets[,"CAC"])
dev.off()

png(file="g77.png", width=600, height=600)
  robust.symetry.plot <- function (x, 
                                   N = max(ceiling(1000/length(x)),2),
                                   alpha = .05, 
                                   xlab = "Distance below the median",
                                   ylab = "Distance above", 
                                   main = "Symetry plot",
                                   ...) {
    cat(N, "\n")
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)], 
          xlab = xlab, ylab = ylab, 
          main = main,
          ... )
    # The symetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn) 
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...) 
    # The 5% confidence interval stemming from the resampled data
    require(quantreg)
    for (tau in c(alpha, 1-alpha)) {
      r <- lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2, lwd = 3)
    # The histogram, in a corner
    op <- par(fig = if (skewness(x)>0) 
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
  robust.symetry.plot(EuStockMarkets[,"CAC"])
dev.off()

png(file="g78.png", width=600, height=600)
  robust.symetry.plot <- function (x, 
                                   N = max(ceiling(1000/length(x)),2),
                                   alpha = .05, 
                                   xlab = "Distance below the median",
                                   ylab = "Distance above the median", 
                                   main = "Symetry plot",
                                   ...) {
    cat(N, "\n")
    # The symetry plot
    x <- x[!is.na(x)]
    n <- length(x)
    nn <- ceiling(n/2)
    x <- sort(x)
    d <- abs(x - median(x))       # Distance to the median
    plot( d[1:nn], d[n:(n-nn+1)], 
          xlab = xlab, ylab = ylab, 
          main = main,
          ... )
    # The symetry plot of resampled, symetric data
    y <- c(x, 2 * median(x) - x)  # We symetrize the data
    X <- Y <- rep(NA, N * nn)
    for (i in 1:N) {
      a <- sort(sample(y, n))
      a <- abs(a - median(a))
      j <- ((i-1) * nn + 1) : (i * nn) 
      X[j] <- a[1:nn]
      Y[j] <- a[n:(n-nn+1)]
    }
    points(X, Y, col="red")
    points( d[1:nn], d[n:(n-nn+1)], ...) 
    # The 5% confidence interval stemming from the resampled data
    require(quantreg)
    for (tau in c(alpha, 1-alpha)) {
      r <- lprq(X, Y,
                h = bw.nrd0(x),  # See ?density
                tau = tau)
      lines(r$xx, r$fv, col = "blue", lwd = 3)
    }
    abline(0, 1, col = "blue", lty = 2)
    # The histogram, in a corner
    op <- par(fig = if (skewness(x)>0) 
                      c(.02,.5,.5,.98)       # Top left corner
                    else c(.5,.98,.02,.5),   # Bottom right
              new = TRUE)
    hist(x, probability=T,
        col="light blue", xlab="", ylab="", main="", axes=F)
    lines(density(x), col="red", lwd=2)
    box()
    par(op)
  }
  robust.symetry.plot(EuStockMarkets[,"FTSE"])
dev.off()

png(file="g79.png", width=600, height=600)
  robust.symetry.plot(rnorm(100), N=100, pch=16)
dev.off()

png(file="g80.png", width=600, height=600)
  data(airquality)
  x <- airquality[,4]
  hist(x, probability=TRUE, breaks=20, col="light blue")
  rug(jitter(x, 5))
  points(density(x), type='l', lwd=3, col='red')
  f <- function(t) {
    dnorm(t, mean=mean(x), sd=sd(x) ) 
  }
  curve(f, add=T, col="red", lwd=3, lty=2)
dev.off()

png(file="g81.png", width=600, height=600)
  x <- airquality[,4]
  qqnorm(x)
  qqline(x,
         col="red", lwd=3)
dev.off()

png(file="g82.png", width=600, height=600)
  y <- rnorm(100)
  qqnorm(y, main="Gaussian random variable")
  qqline(y, 
         col="red", lwd=3)
dev.off()

png(file="g83.png", width=600, height=600)
  y <- rnorm(100)^2
  qqnorm(y, main="Non gaussian variable")
  qqline(y,
         col="red", lwd=3)
dev.off()

png(file="g84.png", width=600, height=600)
  my.qqnorm <- function (x, N=1000, ...) {
    op <- par()
    x <- x[!is.na(x)]
    n <- length(x)
    m <- mean(x)
    s <- sd(x)
    print("a")
    qqnorm(x, axes=F, ...)
    for (i in 1:N) {
      par(new=T)
      qqnorm(rnorm(n, mean=m, sd=s), col='red', pch='.', 
             axes=F, xlab='', ylab='', main='')
    }
    par(new=T)
    qqnorm(x, ...)
    qqline(x, col='blue', lwd=3)
    par(op)
  }
  my.qqnorm(rnorm(100), 
            main = "QQplot: Gaussian distribution")
dev.off()

png(file="g85.png", width=600, height=600)
  my.qqnorm(runif(100), 
            main = "uniform distribution")
dev.off()

png(file="g86.png", width=600, height=600)
  my.qqnorm(exp(rnorm(100)), 
            main = 'log-normal distribution')
dev.off()

png(file="g87.png", width=600, height=600)
  my.qqnorm(c(rnorm(50), 5+rnorm(50)), 
            main = 'bimodal distribution')
dev.off()

png(file="g88.png", width=600, height=600)
  my.qqnorm(c(rnorm(50), 20+rnorm(50)), 
            main = 'two remote peaks')
dev.off()

png(file="g89.png", width=600, height=600)
  x <- rnorm(100)
  x <- x + x^3
  my.qqnorm(x, main = 'fat tails')
dev.off()

png(file="g90.png", width=600, height=600)
  y <- exp(rnorm(100))
  qqnorm(y, 
         main = '(1) Log-normal distribution')
  qqline(y, 
         col = 'red', lwd = 3)
dev.off()

png(file="g91.png", width=600, height=600)
  y <- rnorm(100)^2
  qqnorm(y, ylim = c(-2,2), 
         main = "(2) Square of a gaussian variable")
  qqline(y, 
         col = 'red', lwd = 3)
dev.off()

png(file="g92.png", width=600, height=600)
  y <- -exp(rnorm(100))
  qqnorm(y, ylim = c(-2,2), 
         main = "(3) Opposite of a log-normal variable")
  qqline(y, 
         col = 'red', lwd = 3)
dev.off()

png(file="g93.png", width=600, height=600)
  y <- runif(100, min=-1, max=1)
  qqnorm(y, ylim = c(-2,2), 
         main = '(4) Uniform distribution')
  qqline(y, 
         col = 'red', lwd = 3)
dev.off()

png(file="g94.png", width=600, height=600)
  y <- rnorm(10000)^3
  qqnorm(y, ylim = c(-2,2), 
         main = "(5) Cube of a gaussian r.v.")
  qqline(y, 
         col = 'red', lwd = 3)
dev.off()

png(file="g95.png", width=600, height=600)
  y <- c(rnorm(50), 5+rnorm(50))
  qqnorm(y, 
         main = '(6) Two peaks')
  qqline(y, 
         col = 'red', lwd = 3)
dev.off()

png(file="g96.png", width=600, height=600)
  y <- c(rnorm(50), 20+rnorm(50))
  qqnorm(y, 
         main = '(7) Two peaks, farther away')
  qqline(y, 
         col = 'red', lwd = 3)
dev.off()

png(file="g97.png", width=600, height=600)
  y <- sample(seq(0,1,.1), 100, replace=T)
  qqnorm(y, 
         main = '(7) Discrete distribution')
  qqline(y, 
         col = 'red', lwd = 3)
dev.off()

png(file="g98.png", width=600, height=600)
  x <- seq(from=0, to=2, length=100)
  y <- exp(x)-1
  plot( y ~ x, type = 'l', col = 'red',
        xlim = c(-2,2), ylim = c(-2,2), 
        xlab = "Theoretical (gaussian) quantiles", 
        ylab = "Sample quantiles")        
  lines( x~y, type='l', col='green')
  x <- -x
  y <- -y
  lines( y~x, type='l', col='blue', )
  lines( x~y, type='l', col='cyan')
  abline(0,1)
  legend( -2, 2,
          c( "less concentrated on the right",  
             "more concentrates on the right",
             "less concentrated on the left",
             "more concentrated on the left"
           ),
          lwd=3,
          col=c("red", "green", "blue", "cyan")
        )
  title(main="Reading a qqplot")
dev.off()

png(file="g99.png", width=600, height=600)
  op <- par()
  layout( matrix( c(2,2,1,1), 2, 2, byrow=T ),
          c(1,1), c(1,6),
        )
  # The plot
  n <- 100
  y <- rnorm(n)
  x <- qnorm(ppoints(n))[order(order(y))]
  par(mar=c(5.1,4.1,0,2.1))
  plot( y ~ x, col = "blue", 
        xlab = "Theoretical (gaussian) quantiles", 
        ylab = "Sample quantiles" )
  y1 <- scale( rnorm(n)^2 )
  x <- qnorm(ppoints(n))[order(order(y1))]
  lines(y1~x, type="p", col="red")
  y2 <- scale( -rnorm(n)^2 )
  x <- qnorm(ppoints(n))[order(order(y2))]
  lines(y2~x, type="p", col="green")
  abline(0,1)

  # The legend
  par(bty='n', ann=F)
  g <- seq(0,1, length=10)
  e <- g^2
  f <- sqrt(g)
  h <- c( rep(1,length(e)), rep(2,length(f)), rep(3,length(g)) )
  par(mar=c(0,4.1,1,0))
  boxplot( c(e,f,g) ~ h, horizontal=T, 
           border=c("red", "green", "blue"),
           col="white", # Something prettier?
           xaxt='n',
           yaxt='n', 
           )
  title(main="Reading a qqplot")
  par(op)
dev.off()

png(file="g100.png", width=600, height=600)
  y <- rnorm(100)^2
  y <- scale(x)
  y <- sort(x)
  x <- qnorm( seq(0,1,length=length(y)) )
  plot(y~x)
  abline(0,1)
dev.off()

png(file="g101.png", width=600, height=600)
  qq <- function (y, ylim, quantiles=qnorm,
      main = "Q-Q Plot", xlab = "Theoretical Quantiles",
      ylab = "Sample Quantiles", plot.it = TRUE, ...)
  {
    y <- y[!is.na(y)]
    if (0 == (n <- length(y)))
      stop("y is empty")
    if (missing(ylim))
      ylim <- range(y)
    x <- quantiles(ppoints(n))[order(order(y))]
    if (plot.it)
      plot(x, y, main = main, xlab = xlab, ylab = ylab, ylim = ylim,
            ...)
    # From qqline
    y <- quantile(y, c(0.25, 0.75))
    x <- quantiles(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1] - slope * x[1]
    abline(int, slope, ...)
    invisible(list(x = x, y = y))
  }

  y <- runif(100)
  qq(y, quantiles=qunif)
dev.off()

png(file="g102.png", width=600, height=600)
  two.point.line <- function (x1,y1,x2,y2, ...) {
    a1 <- (y2-y1)/(x2-x1)
    a0 <- y1 - a1 * x1
    abline(a0,a1,...)
  }
  trended.probability.plot <- function (x, q=qnorm) {
    n <- length(x)
    plot( sort(x) ~ q(ppoints(n)), 
          xlab='theoretical quantiles', 
          ylab='sample quantiles')
    two.point.line(q(.25), quantile(x,.25), 
                   q(.75), quantile(x,.75), col='red')
  }
  detrended.probability.plot <- function (x, q=qnorm,
                                          xlab="", ylab="") {
    n <- length(x)
    x <- sort(x)
    x1 <- q(.25)
    y1 <- quantile(x,.25)
    x2 <- q(.75)
    y2 <- quantile(x,.75)
    a1 <- (y2-y1)/(x2-x1)
    a0 <- y1 - a1 * x1
    u <- q(ppoints(n))
    x <- x - (a0 + a1 * u)
    plot(x ~ u, 
         xlab=xlab, ylab=ylab)
    abline(h=0, col='red')
  }

  op <- par(mfrow = c(3,2), mar = c(2,2,2,2) + .1)
  x <- runif(20)
  trended.probability.plot(x)
  detrended.probability.plot(x)
  x <- runif(500)
  trended.probability.plot(x)
  detrended.probability.plot(x)
  trended.probability.plot(x, qunif)
  detrended.probability.plot(x,qunif)
  par(op)
  mtext("Detrended quantile-quantile plots", 
        side=3, line=3, font=2, size=1.5)
dev.off()

png(file="g103.png", width=600, height=600)
  xy <- matrix(c( 0, 0,
                 .2, .9, 
                 .3, .95, 
                 .5, .99,
                  1, 1), byrow = T, nc = 2)
  plot(xy, type = 'b', pch = 15,
       main = "Conventration curve",
       xlab = "patients",
       ylab = "expenses")
  polygon(xy, border=F, col='pink')
  lines(xy, type='b', pch=15)
  abline(0,1,lty=2)
dev.off()

png(file="g104.png", width=600, height=600)
  x <- c(0,20,2720,3000)/3000
  y <- c(0,100000,100100,100110)/100110
  plot(x,y, type='b', pch=15,
       xlab = "Genes", ylab = "ARNm",
       main = "Conventration curve")
  polygon(x,y, border=F, col='pink')
  lines(x,y, type='b', pch=15)
  abline(0,1,lty=2)
dev.off()

png(file="g105.png", width=600, height=600)
  library(ineq)
  op <- par(mfrow=c(3,3), mar=c(2,3,3,2)+.1, oma=c(0,0,2,0))
  n <- 500
  set.seed(1)
  plot(Lc(runif(n,0,1)),   
       main="uniform on [0,1]", col='red',
       xlab="", ylab="")
  do.it <- function (x, main="", xlab="", ylab="") { 
    plot(Lc(x), col = "red",
         main=main, xlab=xlab, ylab=ylab)
  }
  do.it(runif(n,0,10),       main="uniform on [0,10]")
  do.it(runif(n,10,11),      main="uniform on [10,11]")
  do.it(rlnorm(n),          main="log-normal")
  do.it(rlnorm(n,0,4),      main="log-normal, wider")
  do.it(abs(rcauchy(n,1)),  main="half-Cauchy")
  do.it(abs(rnorm(n,1)),    main="half-Gaussian")
  do.it(rpois(n,1),         main="Poisson with mean 1")
  do.it(rpois(n,10),        main="Poisson with mean 10")
  par(op)
  mtext("Gini concentration curves", side=3, line=3, 
        font=2, cex=1.5)
dev.off()

png(file="g106.png", width=600, height=400)
  data(esoph)
  dotchart(table(esoph$agegp))
  mtext("Misleading plot", side=3, line=2.5, font=2, cex=1.2)
  mtext("The origin is not on the plot", side=3, line=1)
dev.off()

png(file="g107.png", width=600, height=600)
  barplot(table(esoph$agegp))
dev.off()

png(file="g108.png", width=600, height=600)
  hist(as.numeric(esoph$agegp), 
       breaks=seq(.5,.5+length(levels(esoph$agegp)),step=1),
       col='light blue')
dev.off()

png(file="g109.png", width=600, height=200)
  boxplot(as.numeric(esoph$agegp), 
          horizontal = T, col = "pink")
dev.off()

png(file="g110.png", width=600, height=200)
  stripchart(jitter(as.numeric(esoph$agegp),2), method='jitter')
dev.off()

png(file="g111.png", width=600, height=600)
  plot(table(esoph$agegp), type='b', pch=7)
dev.off()

png(file="g112.png", width=600, height=600)
  data(HairEyeColor)
  x <- apply(HairEyeColor, 2, sum)
  barplot(x)
  title(main="Column plot")
dev.off()

png(file="g113.png", width=600, height=600)
  barplot(x, col = 1, density = c(3,7,11,20), 
          angle = c(45,-45,45,-45))
  title(main = "Column plot")
dev.off()

png(file="g114.png", width=200, height=600)
  x <- apply(HairEyeColor, 2, sum)
  barplot(as.matrix(x), legend.text = TRUE)
  title("Bar plot")
dev.off()

png(file="g115.png", width=600, height=200)
  barplot(as.matrix(x), 
          horiz = TRUE, 
          col = rainbow(length(x)), 
          legend.text = TRUE)
  title(main = "Bar plot")
dev.off()

png(file="g116.png", width=400, height=600)
  op <- par(no.readonly=TRUE)
  par(mar=c(5,4,4,7)+.1)
  barplot(as.matrix(x))
  title("Bar plot, with legend")
  par(xpd=TRUE)  # Do not clip to the drawing area
  lambda <- .025
  legend(par("usr")[2], 
         par("usr")[4],
         names(x),
         fill = grey.colors(length(x)),         
         xjust = 0, yjust = 1
        )
  par(op)
dev.off()

png(file="g117.png", width=600, height=200)
  op <- par(no.readonly=TRUE)
  par(mar=c(3,1,4,7)+.1)
  barplot(as.matrix(x), 
          horiz = TRUE, 
          col = rainbow(length(x)))
  title(main = "Bar plot, with legend")
  par(xpd=TRUE)  # Do not clip to the drawing area
  lambda <- .05
  legend((1+lambda)*par("usr")[2] - lambda*par("usr")[1], 
         par("usr")[4],
         names(x),
         fill = rainbow(length(x)),         
         xjust = 0, yjust = 1
        )
  par(op)
dev.off()

png(file="g118.png", width=600, height=600)
  data(attenu)
  op <- par(las=2) # Write the labels perpendicularly to the axes
  barplot(table(attenu$event))
  title(main="Column plot")
  par(op)
dev.off()

png(file="g119.png", width=600, height=600)
  op <- par(las=2)
  barplot(rev(sort(table(attenu$event))))
  title(main="Pareto Plot")
  par(op)
dev.off()

png(file="g120.png", width=600, height=600)
  # I cannot seem to manage to do it with 
  # the "barplot" function...
  pareto <- function (x, main = "", ylab = "Value") {
    op <- par(mar = c(5, 4, 4, 5) + 0.1,
              las = 2)
    if( ! inherits(x, "table") ) {
      x <- table(x)
    }
    x <- rev(sort(x))
    plot( x, type = 'h', axes = F, lwd = 16,
          xlab = "", ylab = ylab, main = main )
    axis(2)
    points( x, type = 'h', lwd = 12, 
            col = heat.colors(length(x)) )
    y <- cumsum(x)/sum(x)
    par(new = T)
    plot(y, type = "b", lwd = 3, pch = 7, 
         axes = FALSE, 
         xlab='', ylab='', main='')
    points(y, type = 'h')
    axis(4)
    par(las=0)
    mtext("Cumulated frequency", side=4, line=3)
    print(names(x))
    axis(1, at=1:length(x), labels=names(x))
    par(op)
  }
  pareto(attenu$event)    
  title(main="Pareto plot with cumulated frequencies")
dev.off()

png(file="g121.png", width=600, height=600)
  x <- apply(HairEyeColor, 2, sum)
  pie(x)
  title(main="Pie chart")
dev.off()

png(file="g122.png", width=600, height=800)
  op <- par(mfrow=c(4,2), mar=c(2,4,2,2))

  # Barchart (1 bar)
  set.seed(1)
  x <- rlnorm(6)
  barplot(as.matrix(x), 
          xlim = c(-2,3),
          main = "Barchart")

  # Barchart with an added dimension (stacked area chart) (p.173)
  y <- matrix(rnorm(60), nc=6)
  y <- apply(y, 2, cumsum)
  y <- exp(y/5)
  stacked_area_chart <- function (y, axes = TRUE, ...) {
    stopifnot(all(y>=0))
    y <- t(apply(y, 1, cumsum))
    plot.new()
    plot.window(xlim = c(1,nrow(y)), 
                ylim = range(y) + .1*c(-1,1)*diff(range(y)))
    for (i in ncol(y):1) {
      polygon(c(1,1:nrow(y),nrow(y)), 
              c(0,y[,i],0),
              col=i, border=NA)
      lines(1:nrow(y), y[,i], lwd=3)
    }
    if (axes) {
      axis(1)
      axis(2)
    }
    box()
  }
  stacked_area_chart(y, axes = FALSE)
  title(main = "Barchart with an added dimension",
        sub = "Stacked area chart")

  # Pie chart
  pie(x, 
      col = 1:length(x), 
      labels = LETTERS[1:length(x)],
      main = "Pie chart")

  # Annular chart
  annular_chart <- function (x, r1=1, r2=2) {
    stopifnot(x>=0, r1 >= 0, r2 > 0, r1 < r2)
    x <- cumsum(x) / sum(x)
    x <- c(0,x)
    plot.new()
    plot.window(xlim = c(-1.1,1.1)*r2,
                ylim = c(-1.1,1.1)*r2)
    for (i in 2:length(x)) {
      theta <- 2*pi*seq(x[i-1], x[i], length=100)
      polygon( c(r1 * cos(theta), r2 * cos(rev(theta))),
               c(r1 * sin(theta), r2 * sin(rev(theta))),
               col = i )
    }
  }
  annular_chart(x)
  title("Annular chart")

  # Pie chart
  pie(x, 
      col = 1:length(x), 
      labels = LETTERS[1:length(x)],
      main = "From bad...")

  # Concentrical chart
  # Grid graphics would be better for this: they would
  # help you enforce orthonormal coordinates, and thus
  # circular circles...
  circular_pie <- function (x, ...) {
    stopifnot(is.vector(x), 
              all(x >= 0),
              length(x) >= 1)
    plot.new()
    radii <- sqrt(cumsum(x)) # The areas are 
                             # proportional to the 
                             # inital x
    plot.window(xlim = max(radii)*c(-1.1,1.1),
                ylim = max(radii)*c(-1.1,1.1) )
    theta <- seq(0, 2*pi, length=100)[-1]
    x <- cos(theta)
    y <- sin(theta)
    for (i in length(x):1) {
      polygon(radii[i] * x, radii[i] * y,
              col = i, border = NA)
      lines(radii[i] * x, radii[i] * y)
    }                
  }
  circular_pie(x)
  title("...to worse")

  # barchart (several bars)
  xx <- sample(x)
  barplot(cbind("1" = x, "2" = xx), 
          space = 1, 
          xlim = c(0,5),
          col = 1:length(x),
          main = "Barchart with several bars")

  # Several annular charts p.212
  annular_chart_ <- function (x, r1=1, r2=2) {
    stopifnot(x>=0, r1 >= 0, r2 > 0, r1 < r2)
    x <- cumsum(x) / sum(x)
    x <- c(0,x)
    for (i in 2:length(x)) {
      theta <- 2*pi*seq(x[i-1], x[i], length=100)
      polygon( c(r1 * cos(theta), r2 * cos(rev(theta))),
               c(r1 * sin(theta), r2 * sin(rev(theta))),
               col = i )
    }
  }
  two_annular_charts <- function (x, y, 
                                 r1=1, r2=1.9, 
                                 r3=2, r4=2.9) {
    plot.new()
    plot.window(xlim = c(-1.1,1.1)*r4,
                ylim = c(-1.1,1.1)*r4)
    annular_chart_(x, r1, r2)
    annular_chart_(y, r3, r4)
  }
  two_annular_charts(x, xx)
  title("Two annular charts")

  par(op)
dev.off()

png(file="g123.png", width=600, height=600)
  library(ape)
  example(plot.ancestral)
dev.off()

png(file="g124.png", width=600, height=600)
  example(plot.phylo)
dev.off()

png(file="g125.png", width=600, height=600)
  ##
  ## barplot2D(area, colour)
  ##
  ##
  ## The algorithm is not that obvious.
  ##   - Start with a rectangle, representing 100%, to be filled
  ##     by other rectangles.
  ##   - Try to put the first rectangle on the left
  ##   - If it too elongated, try to put two rectangles, on
  ##     top of each other, on the left
  ##   - Go on, until you are satisfied
  ##   - When you have put those rectangles, proceed with the
  ##     remaining of the large rectangle container.
  ## More precisely, we choose the number of rectables to
  ## stack so as to minimize the following penalty:
  ##   penalty for the first rectangle in the stack + penalty for the last
  ## where the penalty of a rectangle is
  ##   ratio - 1.1
  ## where "ratio" is the ratio of the longer side by the smaller.
  ##
  ## Arguments:
  ##  area:      vector, containing positive number (NAs are discarded),
  ##             to be used as the area of the rectangles
  ##  colour:    vector (same length) of strings containing the colours
  ##             You can create it with "rgb", or "cm.colors".
  ##  threshold: The maximum acceptable aspect ratio of the rectangles
  ##  width, height: Dimensions of the initial rectangle.
  ##                 I suggest to plot the picture in a rectangular
  ##                 device, e.g.,
  ##                   pdf(width=6, height=4)
  ##                 but to tell the function that this rectangle is
  ##                 actually a square, i.e.,
  ##                   barplot2D(area, colour, width=1, height=1)
  ##                 so that the cells be horizontal
  ##                 rectangles: you get more space to add
  ##                 labels
  ##
  ## Returns:
  ##   A matrix, one row per cell, containing the x- and
  ##   y-coordinates of the corners of all the cells (first
  ##   eight columns), and the coordinates of the center of
  ##   those cells (last two columns).
  ##   The rows are in one-to-one correspondance with the
  ##   elements of the "area" vector: if there were missing
  ##   values, we have rows of missing values.
  ##   The row names are the same as the names of the "area"
  ##   vector, in the same order.
  ##

  barplot2D <- function (area, colour, 
                         threshold=1.1, 
                         width=1, height=1) {
    stopifnot(is.vector(area), is.vector(colour),
              length(area) == length(colour),
              !all(is.na(area)))
    if (is.null(names(area))) {
      names(area) <- as.character(1:length(area))
    }
    area0 <- area
    if (any(is.na(area))) {
      warning("Discarding NAs")
      i <- which(!is.na(area))
      area <- area[i]
      colour <- colour[i]
    }
    stopifnot(all(area>=0), sum(area)>0)
    i <- order(-area)
    area <- area[i]
    colour <- colour[i]
    n <- length(area)
    res <- matrix(NA, nr=n, nc=8)
    colnames(res) <- as.vector(t(outer(LETTERS[1:4], 1:2, paste, sep="")))
    rownames(res) <- names(area)
    A <- c(0,height)
    B <- c(0,0)
    C <- c(width,0)
    D <- c(width,height)
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    i <- 1
    while (i <= n) {
      lambda <- cumsum(area[i:n]) / sum(area[i:n])
      mu <- area[i]   / cumsum(area[i:n])
      nu <- area[i:n] / cumsum(area[i:n])
      penalty1 <- mu * sum(abs(A-B)) / ( lambda * sum(abs(A-D)) )
      penalty1 <- ifelse(penalty1 <= threshold, 0, penalty1 - threshold)
      penalty2 <- lambda * sum(abs(A-D)) / ( nu * sum(abs(A-B)) )
      penalty2 <- ifelse(penalty2 <= threshold, 0, penalty2 - threshold)
      j <- which.min(penalty1 + penalty2)[1] + i - 1
      cat(i, " => ", j, "\n")
      lambda <- sum(area[i:j]) / sum(area[i:n])
      A1 <- A
      B1 <- B
      C1 <- (1-lambda) * B + lambda * C
      D1 <- (1-lambda) * A + lambda * D
      AA <- C1
      BB <- C
      CC <- D
      DD <- D1
      while (i <= j) {
        lambda <- area[i] / sum(area[i:j])
        B2 <- (1-lambda) * A1 + lambda * B1
        C2 <- (1-lambda) * D1 + lambda * C1
        polygon(rbind(A1, B2, C2, D1), col=colour[i])
        res[i,] <- c(A1, B2, C2, D1)
        A1 <- B2 
        D1 <- C2
        i <- i + 1
      }
      A <- AA
      B <- BB
      C <- CC
      D <- DD
    } # Main loop
    res0 <- matrix(NA, nr=length(area0), nc=10)
    colnames(res0) <- c(colnames(res), "x", "y")
    rownames(res0) <- names(area0)
    res0[ names(area), 1:8] <- res  
    res0[, "x"] <- apply(res0[,c("A1","B1","C1","D1")],1,mean)
    res0[, "y"] <- apply(res0[,c("A2","B2","C2","D2")],1,mean)
    invisible(res0)
  }

  N <- 20
  area <- rlnorm(N)
  names(area) <- LETTERS[1:N]
  value <- rt(N, df=4)
  # Difficult part: compute the colours...
  colour <- cm.colors(255)[
    1 + round(
      254 * (value - min(value, na.rm = TRUE)) /
      diff(range(value, na.rm = TRUE))
    )
  ]
  r <- barplot2D(area, colour)
  title("2-dimensional barplot")
  # Add the labels
  text(r[,"x"], r[,"y"], names(area), cex=.8)
dev.off()

png(file="g126.png", width=600, height=600)
  library(portfolio)
  example(map.market)
dev.off()

png(file="g127.png", width=600, height=600)
  olap <- function (x, i) {
    # Project (drill-up?) a data cube
    y <- x <- apply(x, i, sum)
    if (length(i) > 1) {
      y <- as.vector(x)
      n <- dimnames(x)
      m <- n[[1]]
      for (i in (1:length(dim(x)))[-1]) {
        m <- outer(m, n[[i]], paste)
      }
      names(y) <- m
    }
    y
  }
  col1 <- c("red", "green", "blue", "brown")
  col2 <- c("red", "light coral",
            "green", "light green",
            "blue", "light blue",
            "brown", "rosy brown")
  col3 <- col2[c(1,2,1,2,3,4,3,4,5,6,5,6,7,8,7,8)]
  op <- par(mfrow=c(3,1), mar=c(8,4,0,2), oma=c(0,0,2,0), las=2)
  barplot(olap(Titanic,1),   space=0, col=col1)
  barplot(olap(Titanic,2:1), space=0, col=col2)
  barplot(olap(Titanic,3:1), space=0, col=col3)
  par(op)
  mtext("Region tree", font = 2, line = 3)
dev.off()

png(file="g128.png", width=600, height=600)
  x1 <- olap(Titanic,3:1)
  x2 <- rep(olap(Titanic,2:1), each=dim(Titanic)[3])
  x3 <- rep(olap(Titanic,1),   each=prod(dim(Titanic)[2:3]))
  x4 <- rep(sum(Titanic),      each=prod(dim(Titanic)[1:3]))
  op <- par(mar=c(8,4,4,2))
  barplot(x4, names.arg="", axes = FALSE, col = "light coral")
  barplot(x3, names.arg="", axes = FALSE, col = "light green",  add = TRUE)
  barplot(x2, names.arg="", axes = FALSE, col = "light blue",   add = TRUE)
  barplot(x1, las=2,        axes = FALSE, col = "yellow", add = TRUE)
  mtext("TempleMVV Plot", line=2, font=2, cex=1.2)
  par(op)
dev.off()

png(file="g129.png", width=600, height=200)
  x <- apply(HairEyeColor, 2, sum)
  dotchart(x, main="dotchart")
dev.off()

png(file="g130.png", width=600, height=600)
  library(MASS)  # For the Cars93 data set
  dotchart(table(Cars93$Manufacturer))
dev.off()

png(file="g131.png", width=600, height=1100)
  library(nlme)
  data(Milk)
  dotchart(table(Milk$Cow))
dev.off()

png(file="g132.png", width=600, height=600)
  data(cars)
  plot(cars$dist ~ cars$speed, 
       xlab = "Speed (mph)", 
       ylab = "Stopping distance (ft)", 
       las = 1)
  title(main = "Point cloud")
dev.off()

png(file="g133.png", width=600, height=600)
  plot(cars$dist ~ cars$speed, 
       xlab = "Speed (mph)", 
       ylab = "Stopping distance (ft)", 
       las = 1)
  title(main = "cars data")
  rug(side=1, jitter(cars$speed, 5))
  rug(side=2, jitter(cars$dist, 20))
dev.off()

png(file="g134.png", width=600, height=600)
  op <- par()
  layout( matrix( c(2,1,0,3), 2, 2, byrow=T ),
          c(1,6), c(4,1),
        )

  par(mar=c(1,1,5,2))
  plot(cars$dist ~ cars$speed, 
       xlab='', ylab='',
       las = 1)
  rug(side=1, jitter(cars$speed, 5) )
  rug(side=2, jitter(cars$dist, 20) )
  title(main = "cars data")

  par(mar=c(1,2,5,1))
  boxplot(cars$dist, axes=F)
  title(ylab='Stopping distance (ft)', line=0)

  par(mar=c(5,1,1,2))
  boxplot(cars$speed, horizontal=T, axes=F)
  title(xlab='Speed (mph)', line=1)

  par(op)
dev.off()

png(file="g135.png", width=600, height=600)
  plot(dist ~ speed, data = cars,
       main = "\"cars\" data and regression line")
  abline(lm( dist ~ speed, data = cars), 
         col = 'red')
dev.off()

png(file="g136.png", width=600, height=600)
  plot(cars, 
       xlab = "Speed (mph)", 
       ylab = "Stopping distance (ft)",
       las = 1)
  # lines(loess(dist ~ speed, data=cars), 
  #       col = "red") # Didn't that use to work?
  r <- loess(dist ~ speed, data=cars)
  lines(r$x, r$fitted, col="red")
  title(main = "\"cars\" data and loess curve")
dev.off()

png(file="g137.png", width=600, height=600)
  plot(cars, 
       xlab = "Speed (mph)", 
       ylab = "Stopping distance (ft)",
       las = 1)
  lines(lowess(cars$speed, cars$dist, 
               f = 2/3, iter = 3), 
        col = "red")
  title(main = "\"cars\" data and lowess curve")
dev.off()

png(file="g138.png", width=600, height=600)
  x <- c(15, 9, 75, 90, 1, 1, 11, 5, 9, 8, 33, 11, 11, 
         20, 14, 13, 10, 28, 33, 21, 24, 25, 11, 33)
  # I tried to produce the same with the "stars" 
  # function, with no success.
  clock.plot <- function (x, col = rainbow(n), ...) {
    if( min(x)<0 ) x <- x - min(x)
    if( max(x)>1 ) x <- x/max(x)
    n <- length(x)
    if(is.null(names(x))) names(x) <- 0:(n-1)
    m <- 1.05
    plot(0, 
         type = 'n', # do not plot anything
         xlim = c(-m,m), ylim = c(-m,m), 
         axes = F, xlab = '', ylab = '', ...)
    a <- pi/2 - 2*pi/200*0:200
    polygon( cos(a), sin(a) )
    v <- .02
    a <- pi/2 - 2*pi/n*0:n
    segments( (1+v)*cos(a), (1+v)*sin(a), 
              (1-v)*cos(a), (1-v)*sin(a) )
    segments( cos(a), sin(a), 
              0, 0, 
              col = 'light grey', lty = 3) 
    ca <- -2*pi/n*(0:50)/50
    for (i in 1:n) {
      a <- pi/2 - 2*pi/n*(i-1)
      b <- pi/2 - 2*pi/n*i
      polygon( c(0, x[i]*cos(a+ca), 0),
               c(0, x[i]*sin(a+ca), 0),
               col=col[i] )
      v <- .1
      text((1+v)*cos(a), (1+v)*sin(a), names(x)[i])
    }
  }
  clock.plot(x, 
    main = "Number of visitors to a web site for each hour of the day")
dev.off()

png(file="g139.png", width=600, height=600)
  library(plotrix)
  clock24.plot(x,
               line.col = "blue", 
               lwd = 10)
  # See also polar.plot, radial.plot
dev.off()

png(file="g140.png", width=600, height=600)
  library(circular)
  rose.diag(x)
  # x <- as.circular(rep( 2*pi / 24 * (0:23), x ))
  detach("package:circular")   # redefines "var"...
dev.off()

png(file="g141.png", width=600, height=600)
  # Polar plot to spot seasonal patterns
  x <- as.vector(UKgas)
  n <- length(x)
  theta <- seq(0, by=2*pi/4, length=n)
  plot(x * cos(theta), x * sin(theta),
       type = "l",
       xlab = "", ylab = "",
       main = "UK gas consumption")
  abline(h=0, v=0, col="grey")
  abline(0, 1, col="grey")
  abline(0, -1, col="grey")
  circle <- function (x, y, r, N=100, ...) {
    theta <- seq(0, 2*pi, length=N+1)
    lines(x + r * cos(theta), y + r * sin(theta), ...)
  }
  circle(0,0, 250, col="grey")
  circle(0,0, 500, col="grey")
  circle(0,0, 750, col="grey")
  circle(0,0, 1000, col="grey")
  circle(0,0, 1250, col="grey")
  segments( x[-n] * cos(theta[-n]),
            x[-n] * sin(theta[-n]),
            x[-1] * cos(theta[-1]),
            x[-1] * sin(theta[-1]),
            col = terrain.colors(length(x)),
            lwd = 3)
  text(par("usr")[2], 0, "Winter", adj=c(1,0))
  text(0, par("usr")[4], "Spring", adj=c(0,1))
  text(par("usr")[1], 0, "Summer", adj=c(0,0))
  text(0, par("usr")[3], "Autumn", adj=c(0,0))
  legend("topright", legend = c(1960, 1973, 1986),
         fill = terrain.colors(3))
dev.off()

png(file="g142.png", width=600, height=600)
  conformal_plot <- function (x, y, ...) {
    # To be used when y is thought to be a periodic function of x, 
    # with period 2pi.
    z <- y + 1i * x
    z <- exp(z)
    x <- Re(z)
    y <- Im(z)
    plot(x, y, ...)
  }
  conformal_abline <- function (h=NULL, v=NULL, a=NULL, b=NULL, ...) {
    if (!is.null(a) | ! is.null(b)) {
      stop("Do not set a or b but only h or v")
    }
    if (!is.null(h)) {
      theta <- seq(0, 2*pi, length=200)
      for (i in 1:length(h)) {
        rho <- exp(h[i])
        lines(rho * cos(theta), rho * sin(theta), type = "l", ...)
      }
    }
    if (!is.null(v)) {
      rho <- sqrt(2) * max(abs(par("usr")))
      segments(0, 0, rho * cos(v), rho * sin(v), ...)
    }
  }

  op <- par(mar=c(1,1,3,1))
  x <- as.vector(sunspots)
  conformal_plot(2 * pi * seq(from=0, by=1/(11*12), length=length(x)),
                 x / 100, 
                 type = "l",
                 lwd = 2,
                 col = "blue",
                 xlab = "", ylab = "",
                 main = "Sunspots after conformal transformation")
  conformal_abline(h=seq(0,3, by=.25), col="grey")
  conformal_abline(v = seq(0, 2*pi, length=12), ## 11 years...
                   col = "grey")
  par(op)
dev.off()

png(file="g143.png", width=600, height=600)
  library(lattice)
  y <- cars$dist
  x <- cars$speed
  # vitesse <- shingle(x, co.intervals(x, number=6))
  vitesse <- equal.count(x)
  histogram(~ y | vitesse)
dev.off()

png(file="g144.png", width=600, height=600)
  bwplot(~ y | vitesse, layout=c(1,6))
dev.off()

png(file="g145.png", width=600, height=600)
  densityplot(~ y | vitesse, aspect='xy')
dev.off()

png(file="g146.png", width=600, height=600)
  densityplot(~ y | vitesse, layout=c(1,6))
dev.off()

png(file="g147.png", width=600, height=300)
  y <- cars$dist
  x <- cars$speed
  q <- quantile(x)
  o1 <- x<q[2]
  o2 <- q[2]<x & x<q[3]
  o3 <- q[3]<x & x<q[4]
  o4 <- q[4]<x 
  dotchart(c(median(y[o1]), median(y[o2]), 
             median(y[o3]), median(y[o4])),
           labels = as.character(1:4),
           xlab = "speed", ylab = "distance",
           main = "Before I knew lattice plots")
dev.off()

png(file="g148.png", width=600, height=600)
  my.dotchart <- function (y,x,...) {
    x <- as.matrix(x)
    z <- NULL
    for (i in 1:dim(x)[2]) {
      q <- quantile(x[,i])
      for (j in 1:4) {
        z <- append(z, median(y[ 
          q[j] <= x[,i] & x[,i] <= q[j+1] 
        ]))
        names(z)[length(z)] <- 
          paste(colnames(x)[i], as.character(j))
      }
    }
    dotchart(z, ...)
  }
  my.dotchart(y, x, xlab = "speed", ylab = "distance",
           main = "Before I knew lattice plots")
dev.off()

png(file="g149.png", width=600, height=600)
  my.dotchart <- function (y,x,...) {
    x <- as.matrix(x)
    z <- NULL
    for (i in 1:dim(x)[2]) {
      q <- quantile(x[,i])
      for (j in 1:4) {
        ya <- y[ q[j] <= x[,i] & x[,i] <= q[j+1] ]
        z <- rbind(z, quantile(ya))
        rownames(z)[dim(z)[1]] <- 
          paste(colnames(x)[i], as.character(j))
      }
    }
    dotchart(t(z), ...)
  }
  my.dotchart(y, x, xlab = "speed", ylab = "distance",
           main = "Before I knew lattice plots")
dev.off()

png(file="g150.png", width=600, height=300)
  my.dotchart <- function (y,x,...) {
    x <- as.matrix(x)
    z <- NULL
    for (i in 1:dim(x)[2]) {
      q <- quantile(x[,i])
      for (j in 1:4) {
        ya <- y[ q[j] <= x[,i] & x[,i] <= q[j+1] ]
        z <- rbind(z, quantile(ya))
        rownames(z)[dim(z)[1]] <- 
          paste(colnames(x)[i], as.character(j))
      }
    }
    xmax <- max(z)
    xmin <- min(z)
    n <- dim(z)[1]
    plot( z[,3], 1:n, xlim = c(xmin,xmax), ylim = c(1,n), 
          axes=F, frame.plot = TRUE, pch = '.', 
          ... )
    axis(1)
    axis(2, at=1:n, las=1)
    abline( h=1:n, lty=3 )
    # median
    points( z[,3], 1:n, pch=16, cex=3 )
    # quartiles
    segments( z[,2], 1:n, z[,4], 1:n, lwd=7 )
    # min and max
    segments( z[,1], 1:n, z[,5], 1:n )
  }
  my.dotchart(y,x, xlab="speed", ylab="distance",
              main = "Before I knew lattice plots")
dev.off()

png(file="g151.png", width=600, height=600)
  plot(cars)
  polygon( cars[chull(cars),], col="pink", lwd=3)
  points(cars)
dev.off()

png(file="g152.png", width=600, height=600)
  draw.ellipse <- function (
      x, y = NULL, 
      N = 100,
      method = lines, 
      ...
    ) {
    if (is.null(y)) {
      y <- x[,2]
      x <- x[,1]
    }
    centre <- c(mean(x), mean(y))
    m <- matrix(c(var(x),cov(x,y),
                  cov(x,y),var(y)),
                nr=2,nc=2)
    e <- eigen(m)
    r <- sqrt(e$values)
    v <- e$vectors
    theta <- seq(0,2*pi, length=N)
    x <- centre[1] + r[1]*v[1,1]*cos(theta) +
         r[2]*v[1,2]*sin(theta)
    y <- centre[2] + r[1]*v[2,1]*cos(theta) +
         r[2]*v[2,2]*sin(theta)
    method(x,y,...)
  }
  plot(cars)
  draw.ellipse(cars, col="blue", lwd=3)
dev.off()

png(file="g153.png", width=600, height=600)
  library(chplot)
  data(hdr)
  x <- hdr$age
  y <- log(hdr$income)
  library(MASS)
  z <- kde2d(x,y, n=50)
  image(z, main = "Density estimation")
dev.off()

png(file="g154.png", width=600, height=600)
  contour(z, 
          col = "red", drawlabels = FALSE,
          main = "Density estimation: contour plot")
dev.off()

png(file="g155.png", width=600, height=600)
  i <- sample(1:length(x), 1000)
  plot(jitter(x[i]), y[i])
  contour(z, 
          col = "red", lwd = 3, drawlabels = FALSE, 
          add = TRUE,
          main = "Density estimation: contour plot")
dev.off()

png(file="g156.png", width=600, height=600)
  persp(z, main = "Density estimation: perspective plot")
dev.off()

png(file="g157.png", width=600, height=600)
  persp(z, 
        phi = 45, theta = 30, 
        xlab = "age", ylab = "income", zlab = "density",
        main = "Density estimation: perspective plot")
dev.off()

png(file="g158.png", width=600, height=600)
  op <- par(mar=c(0,0,2,0)+.1)
  persp(z, phi = 45, theta = 30, 
        xlab = "age", ylab = "income", zlab = "density", 
        col = "yellow", shade = .5, border = NA,
        main = "Density estimation: perspective plot")
  par(op)
dev.off()

png(file="g159.png", width=600, height=600)
  library(chplot)
  data(hdr)
  x <- hdr$age
  y <- log(hdr$income)
  FUNPerFractile <- function (x, y, N, FUN=mean, ...) {
    y <- cut(y, 
             breaks = quantile(y, seq(0, 1, length = N+1), 
                               na.rm=T), 
             labels = FALSE)
    a <- 1:N
    b <- tapply(x, y, FUN, ...)
    data.frame(a,b)
  }
  MeanPerFractilePlot <- function (x, y, N=20, ...) {
    plot(FUNPerFractile(x,y,N=N,FUN=mean,na.rm=T),...)
  }
  MeanPerFractilePlot(x, y, type = "b", lwd = 3, 
                      xlab = "age fractiles", 
                      ylab = "mean income",
                      main = "Mean-per-fractile plot")
dev.off()

png(file="g160.png", width=600, height=600)
  MedianPerFractilePlot <- function (x, y, N=20, ...) {
    plot(FUNPerFractile(x,y,N=N,FUN=median,na.rm=T),...)
  }
  MedianPerFractilePlot(x,y, type="b", lwd=3,
                      xlab = "age fractiles", 
                      ylab = "median income",
                      main = "Median-per-fractile plot")
dev.off()

png(file="g161.png", width=600, height=600)
  r <- loess(y~x)
  o <- order(x)
  plot( r$x[o], r$fitted[o], type = "l",
        xlab = "age", ylab = "income",
        main = "loess curve, without the points" )
dev.off()

png(file="g162.png", width=600, height=600)
  r <- loess(y~x)
  o <- order(x)
  plot( jitter(x, amount = .5), y, pch = ".",
        xlab = "age", ylab = "income",
       main = "Loess curve")
  lines(r$x[o], r$fitted[o], col="blue", lwd=3)
  r <- kde2d(x,y)
  contour(r, drawlabels=F, col="red", lwd=3, add=T)
dev.off()

png(file="g163.png", width=600, height=600)
  data(InsectSprays)
  boxplot(count ~ spray, 
          data = InsectSprays,
          xlab = "Type of spray", 
          ylab = "Insect count",
          main = "InsectSprays data", 
          varwidth = TRUE, 
          col = "lightgray")
dev.off()

png(file="g164.png", width=600, height=400)
  my.dotchart <- function (y,x,...) {
    x <- data.frame(x)
    z <- NULL
    cn <- NULL
    for (i in 1:dim(x)[2]) {
      if( is.numeric(x[,i]) ) {
        q <- quantile(x[,i])
        for (j in 1:4) {
          ya <- y[ q[j] <= x[,i] & x[,i] <= q[j+1] ]
          z <- rbind(z, quantile(ya))
          cn <- append(cn, paste(colnames(x)[i], as.character(j)))
        }
      } else {
        for (j in levels(x[,i])) {
          ya <- y[ x[,i] == j ]
          z <- rbind(z, quantile(ya))
          cn <- append(cn, paste(colnames(x)[i], as.character(j)))
        }
      }
    }
    xmax <- max(z)
    xmin <- min(z)
    n <- dim(z)[1]
    plot( z[,3], 1:n, 
          xlim=c(xmin,xmax), ylim=c(1,n), 
          axes=F, frame.plot=T, pch='.', ... )
    axis(1)
    axis(2, at=1:n, labels=cn, las=1)
    abline( h=1:n, lty=3 )
    # median
    points( z[,3], 1:n, pch=16, cex=3 )
    # quartiles
    segments( z[,2], 1:n, z[,4], 1:n, lwd=7 )
    # min and max
    segments( z[,1], 1:n, z[,5], 1:n )
  }
  spray <- InsectSprays$spray
  y <- InsectSprays$count
  my.dotchart(y,spray, xlab="count", ylab="spray")
dev.off()

png(file="g165.png", width=600, height=600)
  # (This package used to be called UsingR)
  library(UsingR)
  n <- 1000
  k <- 10
  x <- factor(sample(1:5, n, replace=T))
  m <- rnorm(k,sd=2)
  s <- sample( c(rep(1,k-1),2) )
  y <- rnorm(n, m[x], s[x])
  simple.violinplot(y~x, col='pink')
  detach("package:UsingR")
dev.off()

png(file="g166.png", width=600, height=600)
  library(vioplot)
  vioplot(y[x=="1"], y[x=="2"], y[x=="3"], 
          y[x=="4"], y[x=="5"])
  title( main = "vioplot" )
  # The following does not work because the function 
  # wants its first argument to be called "x": it was 
  # defined as function(x,...) instead of function(...).
  # do.call("vioplot", tapply(y, x, function (x) x))
dev.off()

png(file="g167.png", width=600, height=600)
  library(lattice)
  bwplot( y ~ x, 
          panel = panel.violin,
          main = "panel.violin" )
dev.off()

png(file="g168.png", width=600, height=300)
  f <- function (x, N=20) {
    plot.new()
    plot.window( xlim = range(x), 
                 ylim = c(0,1) )
    q <- quantile(x, seq(0, 1, by=1/N))
    segments(q, 0, q, 1)
    lines(c(min(x), min(x), max(x), max(x), min(x)),
            c(0, 1, 1, 0, 0))
  }
  x <- rnorm(1000)
  f(x)
dev.off()

png(file="g169.png", width=600, height=300)
  f <- function (x, N=20) {
    plot.new()
    plot.window( xlim=range(x), ylim=c(-.6,.6) )
    q <- quantile(x, seq(0,1, by=1/N))
    for (i in 1:N) {
      y <- if (i <= N/2) (i-1)/N else (N-i)/N
      lines( c(q[i], q[i], q[i+1], q[i+1], q[i]),
             c(y, -y, -y, y, y) )
    }
  }
  f(x, N=100)
dev.off()

png(file="g170.png", width=600, height=600)
  op <- par(mfrow=c(3,1), mar=c(2,2,3,2))

  n <- length(x)
  plot(sort(x), (1:n)/n, 
       type = "l",
       main = "Cumulative distribution function")

  a <- sort(x)
  b <- (1:length(x))/length(x)
  plot(a, b, 
       type = "l",
       main = "We reverse its second half")
  k <- ceiling(n/2)
  lines(a, c( b[1:(k-1)], (1-b)[k:n] ), 
        col = "blue", lwd = 3)

  plot.new()
  plot.window( xlim=range(x), ylim=c(-.6, .6) )
  lines(a, c( b[1:(k-1)], (1-b)[k:n] ), 
        col="blue", lwd=3,)
  lines(a, -c( b[1:(k-1)], (1-b)[k:n] ), 
        col="blue", lwd=3)
  axis(1)
  title("We symetrize it to get the box-percentile plot")
  abline(h=0, lty=3)
  par(op)
dev.off()

png(file="g171.png", width=600, height=600)
  library(Hmisc)
  bpplot(tapply(y, x, function (x) x))
dev.off()

png(file="g172.png", width=600, height=600)
  library(Hmisc)
  bpplt()    # This is the documentation
  title(main = "bpplt()")
dev.off()

png(file="g173.png", width=600, height=600)
  bwplot( ~ y | x, 
          panel = panel.bpplot, 
          main = "panel.bpplot",
          layout = c(1,5) )
dev.off()

png(file="g174.png", width=600, height=600)
  bpplot( faithful$waiting,
          main = "Box-precentile plot of bimodal data" )
dev.off()

png(file="g175.png", width=400, height=600)
  library(hdrcde)
  hdr.boxplot(rnorm(1000), col = "pink",
              main = "Highest Density Region Plot")
dev.off()

png(file="g176.png", width=400, height=600)
  hdr.boxplot(faithful$waiting, 
              col = "pink",
              main = "Highest Density Region Plot")
dev.off()

png(file="g177.png", width=600, height=600)
  stripchart(InsectSprays$count ~ InsectSprays$spray, 
             method = 'jitter')
dev.off()

png(file="g178.png", width=600, height=600)
  data(iris)
  plot(iris[1:4], 
       pch = 21, 
       bg = c("red", "green", "blue")[
         as.numeric(iris$Species)
       ])
dev.off()

png(file="g179.png", width=600, height=600)
  a <- InsectSprays$count
  b <- rnorm(length(a))
  plot(b ~ a, 
       pch = 21, 
       bg = c("red", "green", "blue", 
            "cyan", "yellow", "black")
         [as.numeric(InsectSprays$spray)],
       main = "1-dimensional scatter plot",
       xlab = "Number of insects",
       ylab = "")
dev.off()

png(file="g180.png", width=600, height=600)
  a <- as.vector(t(iris[1]))
  b <- rnorm(length(a))
  plot(b ~ a, 
       pch = 21, 
       bg = c("red", "green", "blue")[
         as.numeric(iris$Species)
       ],
       main = "1-dimensional scatter plot",
       xlab = "Number of insects",
       ylab = "")
dev.off()

png(file="g181.png", width=600, height=600)
  do.it <- function (v, ...) {
    n <- 100
    y <- sample( 1:3, n, replace=T )
    a <- runif(1)
    b <- runif(1)
    c <- runif(1)
    x <- ifelse( y==1, a+v*rnorm(n), 
         ifelse( y==2, b+v*rnorm(n), c+v*rnorm(n) ))
    r <- rnorm(n)
    plot( r ~ x, 
          pch = 21, 
          bg = c('red', 'green', 'blue')[y],
          ... )
  }
  do.it(.1, main = "1-dimensional scatterplot")
dev.off()

png(file="g182.png", width=600, height=600)
  do.it(.05, main = "1-dimensional scatterplot")
dev.off()

png(file="g183.png", width=400, height=800)
  hists <- function (x, y, ...) {
    y <- factor(y)
    n <- length(levels(y))
    op <- par( mfcol=c(n,1), mar=c(2,4,1,1) )    
    b <- hist(x, ..., plot=F)$breaks
    for (l in levels(y)){
      hist(x[y==l], breaks=b, probability=T, ylim=c(0,.3), 
           main="", ylab=l, col='lightblue', xlab="", ...)
      points(density(x[y==l]), type='l', lwd=3, col='red')
    }
    par(op)
  }
  hists(InsectSprays$count, InsectSprays$spray)
dev.off()

png(file="g184.png", width=600, height=600)
  library(lattice)
  histogram( ~ count | spray, data=InsectSprays)
dev.off()

png(file="g185.png", width=600, height=600)
  densityplot( ~ count | spray, data = InsectSprays )
dev.off()

png(file="g186.png", width=600, height=600)
  bwplot( ~ count | spray, data = InsectSprays )
dev.off()

png(file="g187.png", width=600, height=600)
  bwplot( ~ count | spray, data = InsectSprays, layout=c(1,6) )
dev.off()

png(file="g188.png", width=600, height=600)
  data(HairEyeColor)
  a <- as.table( apply(HairEyeColor, c(1,2), sum) )
  barplot(a, legend.text = attr(a, "dimnames")$Hair)
dev.off()

png(file="g189.png", width=600, height=600)
  barplot(a, 
          beside = TRUE, 
          legend.text = attr(a, "dimnames")$Hair)
dev.off()

png(file="g190.png", width=600, height=600)
  barplot(t(a), 
          legend.text = attr(a, "dimnames")$Eye)
dev.off()

png(file="g191.png", width=600, height=600)
  barplot(t(a), 
          beside = TRUE, 
          legend.text = attr(a, "dimnames")$Eye)
dev.off()

png(file="g192.png", width=600, height=600)
  b <- a / apply(a, 1, sum)
  barplot(t(b))
dev.off()

png(file="g193.png", width=600, height=600)
  c <- t( t(a) / apply(a, 2, sum) )
  barplot(c)
dev.off()

png(file="g194.png", width=600, height=600)
  plot(a, main = "Mosaic plot")
dev.off()

png(file="g195.png", width=600, height=600)
  plot(t(a), main = "Mosaic plot")
dev.off()

png(file="g196.png", width=600, height=600)
  plot(a, 
       col = heat.colors(dim(a)[2]),
       main = "Mosaic plot")
dev.off()

png(file="g197.png", width=600, height=600)
  plot(a, 
       color = TRUE,
       main = "Mosaic plot")
dev.off()

png(file="g198.png", width=600, height=600)
  plot(a, 
       shade = TRUE,
       main = "Mosaic plot")
dev.off()

png(file="g199.png", width=600, height=600)
  plot(t(a), 
       shade = TRUE,
       main = "Mosaic plot")
dev.off()

png(file="g200.png", width=600, height=600)
  data(HairEyeColor)
  a <- apply(HairEyeColor, c(1,2) , sum)
  qualplot <- function (a) {
    matplot( row(a), a, 
             type = 'l', axes = FALSE,
             col = 1:dim(a)[2]+1,
             lty = 1:dim(a)[2],
             lwd=3,
             xlab = names(dimnames(a))[1], 
             ylab = names(dimnames(a))[2] )
    axis(1, 1:dim(a)[1], row.names(a))
    axis(2)
    legend(1, max(a), row.names(t(a)),
           lwd = 3, cex = 1.5,
           col = 1:dim(a)[2]+1, 
           lty = 1:dim(a)[2])
  }
  # For interactive use
  qualplots <- function (a) {
    op <- par(ask=TRUE)
    qualplot(a)
    qualplot(t(a))
    par(op)
  }
  qualplot(a)
dev.off()

png(file="g201.png", width=600, height=600)
  qualplot(t(a))
dev.off()

png(file="g202.png", width=600, height=600)
  qualplotfreq <- function (a) {
    a <- t( t(a) / apply(a,2,sum) )
    qualplot(a)
  }
  qualplotsfreq <- function (a) {
    op <- par(ask=TRUE)
    qualplotfreq(a)
    qualplotfreq(t(a))
    par(op)
  }
  qualplotfreq(a)
dev.off()

png(file="g203.png", width=600, height=600)
  qualplotfreq(t(a))
dev.off()

png(file="g204.png", width=600, height=600)
  data(bacteria, package="MASS")
  fourfoldplot( table(bacteria$y, bacteria$ap) )
dev.off()

png(file="g205.png", width=600, height=600)
  fourfoldplot( table(bacteria$y, 
                      bacteria$ap, 
                      bacteria$week) )
dev.off()

png(file="g206.png", width=600, height=600)
  n <- 50
  x <- rnorm(n)
  y <- rnorm(n)
  z <- rnorm(n)
  my.renorm <- function (z) {
    z <- abs(z)
    z <- 10*z/max(z)
    z
  }
  z <- my.renorm(z)
  op <- par(mar = c(3,2,4,2)+.1)
  plot(x, y, cex = z,
       xlab = "", ylab = "", 
       main = "Bubble plot")
dev.off()

png(file="g207.png", width=600, height=600)
  plot(x, y, cex = z, 
       pch = 16, col = 'red',
       xlab = "", ylab = "", 
       main = "Bubble plot")
  points(x, y, cex = z)
dev.off()

png(file="g208.png", width=600, height=600)
  u <- sample(c('red','green','blue'),n,replace=T)
  plot(x, y, cex = z, col = u,
       pch = 16,
       xlab = "", ylab = "", 
       main = "Bubble plot")
  points(x, y, cex = z)
dev.off()

png(file="g209.png", width=600, height=600)
  z2 <- rnorm(n)
  z2 <- my.renorm(z2)
  plot(x, y, cex = z,
       xlab = "", ylab = "", 
       main = "Bubble plot")
  points(x, y, cex = z2, col = 'red')
dev.off()

png(file="g210.png", width=600, height=600)
  # Other renormalization (if there is no zero)
  my.renorm <- function (z) { 
    z <- (z-min(z)) / (max(z)-min(z))
    z <- 1+9*z
    z
  }
  z <- my.renorm(z)
  z2 <- my.renorm(z2)
  plot(x, y, cex = z,
       xlab = "", ylab = "", 
       main = "Bubble plot")
  points(x, y, cex = z2, col = 'red')
dev.off()

png(file="g211.png", width=600, height=600)
  n <- 50
  x <- runif(n)
  y <- runif(n)
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  z3 <- rnorm(n)
  z4 <- rnorm(n)
  z5 <- rnorm(n)
  stars( data.frame(z1,z2,z3,z4,z5), location=cbind(x,y), 
         labels=NULL, len=1/sqrt(n)/2,
         main = "Star plot" )
dev.off()

png(file="g212.png", width=600, height=600)
  v <- .2
  n <- 50
  x <- runif(n)
  y <- runif(n)
  z1 <- x+y+v*rnorm(n)
  z2 <- x*y+v*rnorm(n)
  z3 <- x^2 + y^2 + v*rlnorm(n)
  stars( data.frame(z1,z2,z3), 
         location = cbind(x,y), 
         labels = NULL, 
         len = 1/sqrt(n)/2, 
         axes = TRUE,
         draw.segments = TRUE, 
         col.segments = 1:5,
         main = "Star plot" )
dev.off()

png(file="g213.png", width=600, height=600)
  n <- 10
  d <- data.frame(y1 = abs(rnorm(n)),
                  y2 = abs(rnorm(n)),
                  y3 = abs(rnorm(n)),
                  y4 = abs(rnorm(n)),
                  y5 = abs(rnorm(n))
                 )
  matplot(d, 
          type = 'l',
          ylab = "", 
          main = "Matplot")
dev.off()

png(file="g214.png", width=600, height=600)
  barplot(t(as.matrix(d)))
dev.off()

png(file="g215.png", width=600, height=600)
  line.chart <- function (d, 
                          xlab = "", ylab = "", 
                          main = "") {
    m <- d
    m <- t(apply(m,1,cumsum))
    #print(m)
    n1 <- dim(m)[1]
    n2 <- dim(m)[2]
    col <- rainbow(n)
    plot.new()
    plot.window(xlim = c(1, n1), 
                ylim = c(min(m), max(m)))
    axis(1)
    axis(2)
    title(xlab = xlab, ylab = ylab,
          main = main)
    for (i in n2:1) {
      polygon(c(1:n1,n1,1), c(m[,i],0,0), 
              col = col[i], 
              border = 0)
    }
    for (i in n2:1) {
      lines(m[,i], lwd = 2)
    }
  }
  line.chart(d, main = "Linechart")
dev.off()

png(file="g216.png", width=600, height=600)
  data(LifeCycleSavings)
  plot(LifeCycleSavings)
dev.off()

png(file="g217.png", width=600, height=600)
  panel.hist <- function(x, ...) {
    usr <- par("usr"); 
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, ...)
  }
  # Correlation coefficient
  my.panel.smooth <- 
  function (x, y, 
            col = par("col"), 
            bg = NA, 
            pch = par("pch"),
            cex = 1, 
            col.smooth = "red", 
            span = 2/3, 
            iter = 3, ...) {
    points(x, y, 
           pch = pch, col = col, 
           bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
      lines(lowess(x[ok], y[ok], 
                   f = span, 
                   iter = iter), 
            col = col.smooth,
            ...)
    usr <- par('usr')
    text( (usr[1]+usr[2])/2, (usr[3]+9*usr[4])/10, 
          floor(100*cor(x,y))/100,
          col='blue', cex=3, adj=c(.5,1) )
  }
  pairs(LifeCycleSavings,
        diag.panel  = panel.hist,
        upper.panel = panel.smooth,
        lower.panel = my.panel.smooth,
        gap = 0)
dev.off()

png(file="g218.png", width=600, height=600)
  cor.plot <- function (x, 
                        xlab = "", ylab = "", 
                        main = "") {
    n <- dim(x)[1]
    m <- dim(x)[2]
    N <- 1000
    col = topo.colors(N)
    plot(NA, xlim = c(0,1.2), ylim = c(-1,0),
         xlab = xlab, ylab = ylab, main = main)
    for (i in 1:n) {
      for (j in 1:m) {
        polygon( c((j-1)/m, (j-1)/m, j/m, j/m),
                 -c((i-1)/m, i/m, i/m, (i-1)/m),
                 col = col[ N*(x[i,j]+1)/2 ] )
      }
    }
    for (i in 1:N) {
      polygon( c(1.1, 1.1, 1.2, 1.2),
               -c((i-1)/N, i/N, i/N, (i-1)/N),
               col = col[N-i+1],
               border = NA )
    }
    # Exercice: add a legend
  }

  n <- 200
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  x <- rnorm(n)
  x4 <- x + .1*rnorm(n)
  x5 <- x + .1*rnorm(n)
  y <- 1 + x1 + x4 + rnorm(n)
  d <- data.frame(y,x1,x2,x3,x4,x5)
  op <- par(mar=c(3,3,4,2)+.1)
  cor.plot(cor(d), main = "Correlation plot")
  par(op)
dev.off()

png(file="g219.png", width=600, height=600)
  library(sma)
  plot.cor(cor(d),
           labels = colnames(d),
           main = "plot.cor (in the \"sma\" package)")
dev.off()

png(file="g220.png", width=600, height=600)
  library(ellipse)
  plotcorr(cor(d), main = "plotcorr (in the \"ellipse\" package)")
dev.off()

png(file="g221.png", width=600, height=600)
  uniformize <- function (x) {
    x <- rank(x, na.last="keep")
    x <- (x - min(x, na.rm=TRUE)) / diff(range(x, na.rm=TRUE))
    x
  }
  scagnostic_contour <- function (x, y, ..., FUN = median) {
    x <- uniformize(x)
    y <- uniformize(y)
    require(MASS) # For kde2d()
    r <- kde2d(x, y, ...)
    r$z > FUN(r$z)
  }
  translate <- function (x,i,j,zero=0) {
    n <- dim(x)[1]
    m <- dim(x)[2]
    while (i>0) {
      x <- rbind( rep(zero,m), x[-n,] )
      i <- i - 1
    }
    while (i<0) {
      x <- rbind( x[-1,], rep(zero,m) )
      i <- i + 1
    }
    while (j>0) {
      x <- cbind( rep(zero,n), x[,-m] )
      j <- j - 1
    }
    while (j<0) {
      x <- cbind( x[,-1], rep(zero,n) )
      j <- j + 1
    }
    x
  }
  scagnostic_perimeter <- function (x, y, ...) {
    z <- scagnostic_contour(x, y, ...)
    zz <- z | 
          translate(z,1,0)  | translate(z,0,1)  | 
          translate(z,-1,0) | translate(z,0,-1)
    sum(zz & ! z)
  }
  scagnostic_area <- function (x, y, ...) {
    z <- scagnostic_contour(x, y, ..., FUN = mean)
    sum(z) / length(z)
  }
  connected_components <- function (x) {
    stopifnot(is.matrix(x), is.logical(x))
    m <- dim(x)[1]
    n <- dim(x)[2]
    x <- rbind( rep(FALSE, n+2),
                cbind( rep(FALSE, m), x, rep(FALSE, m) ),
                rep(FALSE, n+2))
    x[ is.na(x) ] <- FALSE
    # Assign a label to each pixel, so that pixels with the same 
    # label be in the same connected component -- but pixels in the 
    # same connected component may have different labels.
    current_label <- 0
    result <- ifelse(x, 0, 0)
    equivalences <- list()
    for (i in 1 + 1:m) {
      for (j in 1 + 1:n) {
        if (x[i,j]) {
          number_of_neighbours <- x[i-1,j-1] + x[i-1,j] + x[i-1,j+1] + x[i,j-1]
          labels <- c( result[i-1,j-1], result[i-1,j],
                       result[i-1,j+1], result[i,j-1] )
          labels <- unique(labels[ labels > 0 ])
          neighbour_label <- max(0,labels)
          if (number_of_neighbours == 0) {
            current_label <- current_label + 1
            result[i,j] <- current_label
          } else if (length(labels) == 1) {
            result[i,j] <- neighbour_label
          } else {
            result[i,j] <- neighbour_label
            equivalences <- append(equivalences, list(labels))
          }
        }
      }
    }
    # Build the matrix containing the equivalences between those labels
    # We just have the matrix of a (non-equivalence) relation: we compute
    # the equivalence relation it generates.
    E <- matrix(FALSE, nr=current_label, nc=current_label)
    for (e in equivalences) {
      stopifnot( length(e) > 1 )
      for (i in e) {
        for (j in e) {
          if (i != j) {
            E[i,j] <- TRUE
          }
        }
      }
    }
    E <- E | t(E)
    diag(E) <- TRUE
    for (k in 1:current_label) {
      E <- E | (E %*% E > 0)
    }
    stopifnot( E == E | (E %*% E > 0) )
    # Find the equivalence classes, i.e., the unique rows of this matrix
    E <- apply(E, 2, function (x) min(which(x)))
    # Finally, label the equivalence classes
    for (i in 1:current_label) {
      result[ result == i ] <- E[i]
    }
    result
  }
  connected_components_TEST <- function () {
    n <- 100
    x <- matrix(NA, nr=n, nc=n)
    x <- abs(col(x) - (n/3)) < n/8 & abs(row(x) - n/3) < n/8
    x <- x | ( (col(x) - 2*n/3)^2 + (row(x) - 2*n/3)^2 < (n/8)^2 )
    image(!x)
    image(-connected_components(x))    
  }
  scagnostic_modality <- function (x, y, ...) {
    z <- scagnostic_contour(x, y, ...)
    z <- connected_components(z)
    max(z)
  }
  scagnostic_slope <- function (x,y) {
    x <- uniformize(x)
    y <- uniformize(y)
    pc1 <- prcomp(cbind(x,y))$rotation[,1]
    pc1[2] / pc1[1]
  }
  scagnostic_sphericity <- function (x,y) {
    x <- uniformize(x)
    y <- uniformize(y)
    # Ratio of the eigenvalues of the PCA
    # For a spherical cloud of points, the slope
    # is not well defined, but this ratio is close to 1.
    ev <- prcomp(cbind(x,y))$sdev
    ev[1] / ev[2]
  }
  scagnostic_curvature <- function (x,y) {
    x <- uniformize(x)
    y <- uniformize(y)
    require(pcurve)
    # BUG: pcurve() starts a new plot by fiddling with par() --
    # it also fails to set it back to what it was...
    par <- function (...) { }
    r <- NULL
    try(
    r <- pcurve(cbind(x,y),
                start = "pca", # Defaults to CA,
                               # which only works with count data...
                plot.pca = FALSE,
                plot.true = FALSE,
                plot.init = FALSE, 
                plot.segs = FALSE, 
                plot.resp = FALSE,
                plot.cov = FALSE,
                use.loc = FALSE)
    )
    if (is.null(r)) return(0)
    X <- r$s[,1:2]               # The principal curve
    n <- dim(X)[1]
    V <- X[2:n,] - X[1:(n-1),]
    V <- V / sqrt(V[,1]^2 + V[,2]^2) # The direction of the principal 
                                     # curve, at each point on it
    C <- apply( V[1:(n-2),] * V[2:(n-1),], 1, sum )
    C <- acos(C)                 # The angles
    sum(abs(C)) / pi
  }
  scagnostic_distance <- function (x,y) {
    i <- is.finite(x) & is.finite(y)
    if (length(i) < 2) {
      return(NA)
    }
    x <- uniformize(x)[i]
    y <- uniformize(y)[i]
    d <- as.matrix(dist(cbind(x,y)))
    diag(d) <- Inf
    d <- apply(d, 2, min)   # Nearest neighbour distance
    mean(d)
  }
  scagnostics <- function (
    x, 
    functions = list(
      Perimeter  = scagnostic_perimeter,
      Area       = scagnostic_area,
      Modality   = scagnostic_modality,
      Slope      = scagnostic_slope,
      Sphericity = scagnostic_sphericity,
      Curvature  = scagnostic_curvature,
      "Nearest neighbour distance" = scagnostic_distance
    )
  ) {
    stopifnot( is.matrix(x) || is.data.frame(x) )
    number_of_variables   <- dim(x)[2]
    number_of_scagnostics <- length(functions)
    res <- array(NA, dim=c(number_of_variables,
                           number_of_variables,
                           number_of_scagnostics))
    dimnames(res) <- list(
      Variable1 = colnames(x),
      Variable2 = colnames(x),
      Scagnostic = names(functions)
    )
    for (i in 1:number_of_variables) {
      for (j in 1:number_of_variables) {
        if (i != j) {
          for (k in 1:number_of_scagnostics) {
            res[i,j,k] <- functions[[k]] (x[,i], x[,j])
          }
        }
      }
    }
    class(res) <- "scagnostics"
    res
  }
  plot.scagnostics <- function (x, FUN=pairs, ...) {
    stopifnot(inherits(x, "scagnostics"))
    y <- apply(x, 3, as.vector)
    colnames(y) <- dimnames(x)[[3]]
    rownames(y) <- outer(dimnames(x)[[1]], dimnames(x)[[2]], paste, sep="-")
    FUN(y, ...)
  }

  pairs(USJudgeRatings, gap=0)
dev.off()

png(file="g222.png", width=600, height=600)
  plot(scagnostics(USJudgeRatings), gap=0)
dev.off()

png(file="g223.png", width=600, height=600)
  x <- Harman74.cor[[1]]
  pairs(x, gap=0)
dev.off()

png(file="g224.png", width=600, height=600)
  plot(scagnostics(x), gap=0)
dev.off()

png(file="g225.png", width=600, height=600)
  image(t(as.matrix(USJudgeRatings)))
dev.off()

png(file="g226.png", width=600, height=600)
  # This uses cluster analysis
  heatmap(as.matrix(USJudgeRatings))
dev.off()

png(file="g227.png", width=600, height=600)
  my.dotchart(LifeCycleSavings[,1], LifeCycleSavings[,-1],
              xlab='savings', ylab='')
dev.off()

png(file="g228.png", width=600, height=600)
  to.factor.vector <- function (x, number = 4) {
    resultat <- NULL
    intervalles <- co.intervals(x, number, 
                                overlap = 0)
    for (i in 1:number) {
      if ( i == 1 ) {
        intervalles[i,1] = min(x)
      } else {
        intervalles[i,1] <- intervalles[i-1,2]
      }
      if( i == number ) {
        intervalles[i,2] <- max(x)
      }
    }
    for (valeur in x) {
      r <- NA
      for (i in 1:number) {
        if( valeur >= intervalles[i,1] & 
            valeur <= intervalles[i,2] )
          r <- i
      }
      resultat <- append(resultat, r)
    }
    factor(resultat, levels = 1:number)
  }
  to.factor <- function (x, number = 4) {
    if(is.vector(x)) 
      r <- to.factor.vector(x, number)
    else {
      r <- NULL
      for (v in x) {
        a <- to.factor.vector(v)
        if( is.null(r) ) 
          r <- data.frame(a)
        else 
          r <- data.frame(r,a)
      }
      names(r) <- names(x)
    }
    r
  }
  x <- to.factor(LifeCycleSavings[,-1])
  y <- LifeCycleSavings[,1]
  y <- as.vector(matrix(y, 
                        nr = length(y), 
                        ncol = dim(x)[2]))
  for (i in names(x)) {
    levels(x[[i]]) <- paste(i, levels(x[[i]]))
  }
  col <- gl( dim(x)[2], length(levels(x[,1])), 
             labels = rainbow( dim(x)[2] ))
  col <- as.vector(col)
  x <- factor(as.vector(as.matrix(x)))
  boxplot(y ~ x, 
          horizontal = TRUE, 
          las = 1, 
          col = col,
          main = "Boxplot for each quartile")
dev.off()

png(file="g229.png", width=600, height=300)
  bwplot( ~ LifeCycleSavings[,1] | 
            equal.count(LifeCycleSavings[,2], number=4),
          layout=c(1,4) )
dev.off()

png(file="g230.png", width=600, height=300)
  bwplot( ~ LifeCycleSavings[,1] | 
            equal.count(LifeCycleSavings[,3], number=4),
          layout=c(1,4) )
dev.off()

png(file="g231.png", width=600, height=300)
  bwplot( ~ LifeCycleSavings[,1] | 
            equal.count(LifeCycleSavings[,4], number=4),
          layout=c(1,4) )
dev.off()

png(file="g232.png", width=600, height=300)
  bwplot( ~ LifeCycleSavings[,1] | 
            equal.count(LifeCycleSavings[,5], number=4),
          layout=c(1,4) )
dev.off()

png(file="g233.png", width=600, height=600)
  data(mtcars)
  stars(mtcars[, 1:7], 
        key.loc = c(14, 2),
        main = "Motor Trend Cars : stars(*, full = FALSE)", 
        full = FALSE)
dev.off()

png(file="g234.png", width=600, height=600)
  stars(mtcars[, 1:7], 
        key.loc = c(14, 1.5),
        main = "Motor Trend Cars : full stars()",
        flip.labels = FALSE)
dev.off()

png(file="g235.png", width=600, height=600)
  palette(rainbow(12, s = 0.6, v = 0.75))
  stars(mtcars[, 1:7], 
        len = 0.8, 
        key.loc = c(12, 1.5),
        main = "Motor Trend Cars", 
        draw.segments = TRUE)
dev.off()

png(file="g236.png", width=600, height=600)
  stars(mtcars[, 1:7], 
        locations = c(0,0), 
        radius = FALSE,
        key.loc=c(0,0), 
        main="Motor Trend Cars", 
        lty = 2)
dev.off()

png(file="g237.png", width=600, height=600)
  library(circular)
  rose.diag(mtcars[,5])
dev.off()

png(file="g238.png", width=600, height=600)
  rose.diag(mtcars)
dev.off()

png(file="g239.png", width=600, height=600)
  # From the manual
  x <- seq(-10, 10, length=50)
  y <- x
  f <- function(x,y) {
    r <- sqrt(x^2+y^2)
    10 * sin(r)/r
  }
  z <- outer(x, y, f)
  z[is.na(z)] <- 1
  persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
        shade=.5,
        xlab = "X", ylab = "Y", zlab = "Z")
dev.off()

png(file="g240.png", width=600, height=600)
  # From the manual
  data(volcano)
  z <- 2 * volcano        # Exaggerate the relief
  x <- 10 * (1:nrow(z))   # 10-meter spacing (S to N)
  y <- 10 * (1:ncol(z))   # 10-meter spacing (E to W)
  persp(x, y, z, 
        theta = 120, phi = 15, 
        scale = FALSE, axes = FALSE)
  # See also the other examples in 
  #   demo(persp)
dev.off()

png(file="g241.png", width=600, height=600)
  # From the manual
  data("volcano")
  rx <- range(x <- 10*1:nrow(volcano))
  ry <- range(y <- 10*1:ncol(volcano))
  ry <- ry + c(-1,1) * (diff(rx) - diff(ry))/2
  tcol <- terrain.colors(12)
  op <- par(pty = "s", bg = "lightcyan")
  plot(x = 0, y = 0,
       type = "n", 
       xlim = rx, ylim = ry, 
       xlab = "", ylab = "")
  u <- par("usr")
  rect(u[1], u[3], u[2], u[4], 
       col = tcol[8], 
       border = "red")
  contour(x, y, volcano, 
          col = tcol[2], 
          lty = "solid", 
          add = TRUE,
          vfont = c("sans serif", "plain"))
  title("A Topographic Map of Maunga Whau", font = 4)
  abline(h = 200*0:4, v = 200*0:4, 
         col = "lightgray", 
         lty = 2, 
         lwd = 0.1)
  par(op)
dev.off()

png(file="g242.png", width=600, height=600)
  # From the manual
  data(volcano)
  x <- 10*(1:nrow(volcano))
  y <- 10*(1:ncol(volcano))
  image(x, y, volcano, 
        col = terrain.colors(100),
        axes = FALSE,
        xlab = "", ylab = "")
  contour(x, y, volcano, 
          levels = seq(90, 200, by=5), 
          add = TRUE, 
          col = "peru")
  axis(1, at = seq(100, 800, by = 100))
  axis(2, at = seq(100, 600, by = 100))
  box()
  title(main = "Maunga Whau Volcano", font.main = 4)
dev.off()

png(file="g243.png", width=600, height=600)
  data(volcano)
  x <- 10*(1:nrow(volcano))
  x <- rep(x, ncol(volcano))
  y <- 10*(1:ncol(volcano))
  y <- rep(y, each=nrow(volcano))
  z <- as.vector(volcano)
  wireframe( z ~ x * y )
dev.off()

png(file="g244.png", width=600, height=600)
  cloud( z ~ x * y )
dev.off()

png(file="g245.png", width=600, height=600)
  data(iris)
  print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
              data = iris, cex = .8,
              groups = Species,
              subpanel = panel.superpose,
              main = "Stereo",
              screen = list(z = 20, x = -70, y = 3)),
        split = c(1,1,2,1), more = TRUE)
  print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
              data = iris, cex = .8,
              groups = Species,
              subpanel = panel.superpose,
              main = "Stereo",
              screen = list(z = 20, x = -70, y = 0)),
        split = c(2,1,2,1))
dev.off()

png(file="g246.png", width=600, height=800)
  z <- matrix(rnorm(24),nr=4)
  library(akima) # non-free
  r <- interp( as.vector(row(z)),
               as.vector(col(z)),
               as.vector(z),
               seq(1, dim(z)[1], length=500),
               seq(1, dim(z)[2], length=500) )
  op <- par(mfrow=c(2,1))
  image(t(z), main="Data to be smoothed or interpolated")
  box()
  image(t(r$z), main="Linear interpolation")
  box()
  par(op)
dev.off()

png(file="g247.png", width=600, height=800)
  library(fields)
  loc <- make.surface.grid(list( seq(1,dim(z)[1],length=500),
                                 seq(1,dim(z)[2],length=500) ))
  r <- interp.surface(
    list(x=1:dim(z)[1], y=1:dim(z)[2], z=z),
    loc
  )
  op <- par(mfrow=c(2,1))
  image.plot(z, main="Raw data")
  image.plot(as.surface(loc,r), main="Linear interpolation")
  par(op)
dev.off()

png(file="g248.png", width=600, height=600)
  # You may not want to interpolate, but rather to smooth
  # (the initial data set need not be on a grid)
  # Also check the Tps() function in the fields package
  library(tgp)
  r <- interp.loess( as.vector(row(z)),
                     as.vector(col(z)),
                     as.vector(z),
                     gridlen = 500 )
  image(t(r$z), main="Loess 2-dimensional smoothing")
dev.off()

png(file="g249.png", width=600, height=600)
  library(fields)
  data(lennon)
  x <- lennon[201:240,201:240]
  op <- par(mfrow=c(2,1))
  image(x, main="image()")
  image.plot(x, main="image.plot()")
  par(op)
dev.off()

png(file="g250.png", width=600, height=600)
  library(RColorBrewer)
  display.brewer.all(type="div")
  title(main="RColorBrewer: diverging palettes (i.e., with a zero)")
dev.off()

png(file="g251.png", width=600, height=600)
  display.brewer.all(type="seq")
  title(main="RColorBrewer: sequential palettes")
dev.off()

png(file="g252.png", width=600, height=600)
  display.brewer.all(type="qual")
  title(main="RColorBrewer: qualitative palettes")
dev.off()

png(file="g253.png", width=600, height=600)
  breaks <- function (x, N) {
    x <- as.vector(x)
    x <- x[ !is.na(x) ]
    if (length(x) == 0) {
      return( rep(NA, N) )
    }
    if (N %% 2 == 0) {
      if (all(x >= 0)) {
        res <- c(rep(0, N/2), seq(0, max(x), length=N/2+1))
      } else if (all(x <= 0)) {
        res <- c(seq(min(x), 0, length=N/2+1), rep(0,N/2))
      } else {
        res <- c(seq(min(x), 0, length=N/2+1),
                 seq(0, max(x), length=N/2+1)[-1])
      }
    } else {
      if (all(x >= 0)) {
        res <- c(rep(0,length=(N+1)/2), seq(0, max(x), length=(N+1)/2))
      } else if (all(x <= 0)) {
        res <- c(seq(min(x), 0, length=(N+1)/2), rep(0, length=(N+1)/2))
      } else {
        res <- c(seq(min(x), 0, length=N+1) [seq(1, N, by=2)],
                 seq(0, max(x), length=N+1) [seq(2, N+1, by=2)])
      }
    }
    stopifnot( length(res) == N+1 )
    stopifnot( res == sort(res) )
    stopifnot( all(x <= max(res)), all(x >= min(res)) )
    res
  }

  breaks(  0:10,  5) == c(0,0,0,    0,5,10)
  breaks(-(0:10), 5) == c(-10,-5,0, 0,0,0)
  breaks(-20:10,  5)  == c(-20, -12, -4, 2, 6, 10)
  breaks(   0:9,  6)  == c(0,0,0, 0, 3,6,9)
  breaks(-(0:9),  6)  == c(-9,-6,-3, 0, 0,0,0)
  breaks( -30:9,  6) == c(-30,-20,-10,0,3,6,9)

  # Example from the "fields" manual
  data(ozone2)
  x<-ozone2$lon.lat
  y<- ozone2$y[16,]
  # Remove the missing values
  i <- !is.na(y)
  y <- y[i]
  x <- x[i,]
  # The residuals of a regression
  r <- Tps(x,y)
  z <- residuals(r)
  # Put those residuals on a regular grid
  # We cannot use interp.surface(): it assumes that the data is regular
  library(tgp)
  r <- interp.loess(x[,1], x[,2], z, gridlen=500)

  # I wanted an example with skewed data: residuals tend to be symetric...
  op <- par(mfrow=c(2,2))
  image(r)
  image.plot(r)
  image.plot(r,
             breaks=breaks(r$z, 9), # Fine for the plot, but no for the legend...
             col=rev(brewer.pal(9, "RdBu")))
  par(op)
dev.off()

png(file="g254.png", width=600, height=600)
  n <- 100
  m <- matrix( rnorm(5*n)+c(1,-1,3,0,2), 
               nr = n, nc = 5, byrow = TRUE )
  matplot(1:5, t(m), type = 'l',
          xlab = "", ylab = "")
  title(main = "Parallel plot: Homogeneous cloud")
dev.off()

png(file="g255.png", width=600, height=600)
  n <- 50
  k <- 2
  m <- matrix( rnorm(5*k*n) + 
                 runif(5*k, min = -10, max = 10), 
               nr = n, nc = 5, byrow = TRUE )
  matplot(1:5, t(m), type = 'l',
          xlab = "", ylab = "")
  title(main = "Parallel plot: two clusters")
dev.off()

png(file="g256.png", width=600, height=600)
  n <- 50
  k <- 5
  m <- matrix( rnorm(5*k*n) + 
                 runif(5*k, min = -10, max = 10), 
               nr = n, nc = 5, byrow = TRUE )
  matplot(1:5, t(m), type = 'l',
          xlab = "", ylab = "")
  title(main = "Parallel plot, 5 clusters")
dev.off()

png(file="g257.png", width=600, height=600)
  matplot(1:5, t(princomp(m)$scores), type = 'l')
  title(main = "Idem, after PCA")
dev.off()

png(file="g258.png", width=600, height=600)
  matplot(1:5, t(m), type = 'l')
  title(main = "Point cloud with less visible clusters")
dev.off()

png(file="g259.png", width=600, height=600)
  library(lattice)
  parallel(as.data.frame(m))
dev.off()

png(file="g260.png", width=600, height=600)
  polar_parallel_plot <- function (d, col = par("fg"),
                                   type = "l", lty = 1, ...) {
    d <- as.matrix(d)
    d <- apply(d, 2, function (x) .5 + (x - min(x)) / (max(x) - min(x)))
    theta <- (col(d) - 1) / ncol(d) * 2 * pi
    d <- cbind(d, d[,1])
    theta <- cbind(theta, theta[,1])
    matplot( t(d * cos(theta)), 
             t(d * sin(theta)),
             col = col, 
             type = type, lty = lty, ...,
             axes = FALSE, xlab = "", ylab = "" )
    segments(rep(0,ncol(theta)), rep(0, ncol(theta)),
             1.5 * cos(theta[1,]), 1.5 * sin(theta[1,]))
    if (! is.null(colnames(d))) {
      text(1.5 * cos(theta[1,-ncol(theta)]),
           1.5 * sin(theta[1,-ncol(theta)]),
           colnames(d)[-ncol(d)])
    }
  }
  op <- par(mar=c(0,0,0,0))
  polar_parallel_plot(iris[1:4], col = as.numeric(iris$Species))
  par(op)
dev.off()

png(file="g261.png", width=600, height=600)
  parallel(~iris[1:4], groups = Species, iris)
dev.off()

png(file="g262.png", width=600, height=600)
  parallel(~iris[c(2,4,1,3)], groups= Species, iris)
dev.off()

png(file="g263.png", width=600, height=600)
  x <- seq(-pi, pi, length=100)
  y <- apply(as.matrix(iris[,1:4]),
             1,
             function (u) u[1] + u[2] * cos(x) +
                                 u[3] * sin(x) + u[4] * cos(2*x))
  matplot(x, y,
          type = "l",
          lty = 1,
          col = as.numeric(iris[,5]),
          xlab = "", ylab = "",
          main = "Fourier (Andrew) curves")
dev.off()

png(file="g264.png", width=600, height=600)
  matplot(y * cos(x), y * sin(x),
          type = "l",
          lty = 1,
          col = as.numeric(iris[,5]),
          xlab = "", ylab = "",
          main = "Fourier blob")
dev.off()

png(file="g265.png", width=600, height=600)
  library(TeachingDemos)
  faces(longley[1:9,], main="Macro-economic data")
dev.off()

png(file="g266.png", width=600, height=600)
  library(MASS)
  data(Skye)

  ternary <- function(X, pch = par("pch"), lcex = 1,
                      add = FALSE, ord = 1:3, ...)
  {
    X <- as.matrix(X)
    if(any(X) < 0) stop("X must be non-negative")
    s <- drop(X %*% rep(1, ncol(X)))
    if(any(s<=0)) stop("each row of X must have a positive sum")
    if(max(abs(s-1)) > 1e-6) {
      warning("row(s) of X will be rescaled")
      X <- X / s
    }
    X <- X[, ord]
    s3 <- sqrt(1/3)
    if(!add)
    {
      oldpty <- par("pty")
      on.exit(par(pty=oldpty))
      par(pty="s")
      plot(c(-s3, s3), c(0.5-s3, 0.5+s3), type="n", axes=FALSE,
           xlab="", ylab="")
      polygon(c(0, -s3, s3), c(1, 0, 0), density=0)
      lab <- NULL
      if(!is.null(dn <- dimnames(X))) lab <- dn[[2]]
      if(length(lab) < 3) lab <- as.character(1:3)
      eps <- 0.05 * lcex
      text(c(0, s3+eps*0.7, -s3-eps*0.7),
           c(1+eps, -0.1*eps, -0.1*eps), lab, cex=lcex)
    }
    points((X[,2] - X[,3])*s3, X[,1], ...)
  }

  ternary(Skye/100, ord=c(1,3,2))
dev.off()

png(file="g267.png", width=600, height=600)
  tri <-
  function(a, f, m, symb = 2, grid = F, ...)
  {
    ta <- paste(substitute(a))
    tf <- paste(substitute(f))
    tm <- paste(substitute(m))

    tot <- 100/(a + f +m)
    b <- f * tot
    y <- b * .878
    x <- m * tot + b/2
    par(pty = "s")
    oldcol <- par("col")
    plot(x, y, axes = F, xlab = "", ylab = "", 
         xlim = c(-10, 110), ylim= c(-10, 110), type = "n", ...)
    points(x,y,pch=symb)
    par(col = oldcol)
    trigrid(grid)
    text(-5, -5, ta)
    text(105, -5, tm)
    text(50, 93, tf)
    par(pty = "m")
    invisible()
  }

  trigrid  <-
  function(grid = F)
  {
    lines(c(0, 50, 100, 0), c(0, 87.8, 0, 0)) #draw frame
    if(!grid) {
      for(i in 1:4 * 20) {
        lines(c(i, i - 1), c(0, 2 * .878)) #side a-c (base)
        lines(c(i, i + 1), c(0, 2 * .878))
        T.j <- i/2 #side a-b (left)
        lines(c(T.j, T.j + 2), c(i * .878, i * .878))
        lines(c(T.j, T.j + 1), c(i * .878, (i - 2) * .878))
        T.j <- 100 - i/2 #side b-c (right)
        lines(c(T.j, T.j - 2), c(i * .878, i * .878))
        lines(c(T.j, T.j - 1), c(i * .878, (i - 2) * .878))
      }
    } else {
      for(i in 1:4 * 20) {
        # draw dotted grid
        lines(c(i, i/2), c(0, i * .878), lty = 4, col = 3)
        lines(c(i, (50 + i/2)), c(0, .878 * (100 - i)), lty = 4, col = 3)
        lines(c(i/2, (100 - i/2)), c(i * .878, i * .878), lty = 4, col = 3)
      }
      par(lty = 1, col = 1)
    }
  }

  # some random data in three variables
  c1<- runif(5, 10, 20)
  c2<- runif(5, 1, 5)
  c3 <- runif(5, 15, 25)
  # basic plot
  tri(c1,c2,c3)
dev.off()

png(file="g268.png", width=600, height=600)
  # plot with different symbols and a grid
  tri(c1,c2,c3, symb=7, grid=T) 
dev.off()

png(file="g269.png", width=600, height=600)
  histogram( ~ Sepal.Length | Species, data = iris, 
             layout = c(1,3) )
dev.off()

png(file="g270.png", width=600, height=600)
  xyplot( Sepal.Length ~ Sepal.Width | Species, data = iris,
          layout = c(1,3) )
dev.off()

png(file="g271.png", width=600, height=600)
  xyplot( Sepal.Length ~ Sepal.Width, group = Species, data = iris,
          panel = function (x, y, groups, ...) {
            panel.superpose(x, y, groups = groups, ...)
            groups <- as.factor(groups)
            for (i in seq(along=levels(groups))) {
              g <- levels(groups)[i]
              panel.lmline( x[groups == g], y[groups == g], 
                            col = trellis.par.get("superpose.line")$col[i] )
            }
          }
        )
dev.off()

png(file="g272.png", width=600, height=600)
  xyplot( Sepal.Length ~ Sepal.Width, group = Species, data = iris,
          panel = function (x, y, groups, ...) {
            panel.superpose(x, y, groups = groups, ...)
            groups <- as.factor(groups)
            for (i in seq(along=levels(groups))) {
              g <- levels(groups)[i]
              panel.loess( x[groups == g], y[groups == g], 
                            col = trellis.par.get("superpose.line")$col[i] )
            }
          }
        )
dev.off()

png(file="g273.png", width=600, height=600)
  data(iris)
  plot(iris[1:4], pch=21, 
       bg=c("red", "green", "blue")[as.numeric(iris$Species)])
dev.off()

png(file="g274.png", width=600, height=600)
  a <- rnorm(10)
  b <- 1+ rnorm(10)
  c <- 1+ rnorm(10)
  d <- rnorm(10)
  x <- c(a,b,c,d)
  y <- factor(c( rep("A",20), rep("B",20)))
  z <- factor(c( rep("U",10), rep("V",20), rep("U",10) ))
  op <- par(mfrow=c(2,2))
  plot(x~y)
  plot(x~z)
  plot(x[z=="U"] ~ y[z=="U"], border="red", ylim=c(min(x),max(x)))
  plot(x[z=="V"] ~ y[z=="V"], border="blue", add=T)
  plot(x[y=="A"] ~ z[y=="A"], border="red", ylim=c(min(x),max(x)))
  plot(x[y=="B"] ~ z[y=="B"], border="blue", add=T)
  par(op)
dev.off()

png(file="g275.png", width=600, height=600)
  l <- rep("",length(x))
  for (i in 1:length(x)){
    l[i] <- paste(y[i],z[i])
  }
  l <- factor(l)
  boxplot(x~l)
dev.off()

png(file="g276.png", width=600, height=600)
  # l is a 2-element list
  myplot1 <- function (x, l, ...) {
    t <- tapply(x,l,mean)
    l1 <- levels(l[[1]])
    l2 <- levels(l[[2]])
    matplot(t,
            type='l', lty=1, col=1:length(l2),
            axes=F, ...)
    axis(1, 1:2, l1)
    axis(2)
    lim <- par("usr")
    legend(lim[1] + .05*(lim[2]-lim[1]), lim[4],
           l2, lwd=1, lty=1, col=1:length(l2) )
  }
  op <- par(mfrow=c(1,2))
  myplot1( x, list(y,z), ylim=c(0,2), ylab = "" )
  myplot1( x, list(z,y), ylim=c(0,2), ylab = "" )
  par(op)
dev.off()

png(file="g277.png", width=600, height=600)
  myplot3 <- function (x, l, ...) {
    l1 <- levels(l[[1]])
    l2 <- levels(l[[2]])
    t0 <- tapply(x,l,min)
    t1 <- tapply(x,l,function(x)quantile(x,.25))
    t2 <- tapply(x,l,median)
    t3 <- tapply(x,l,function(x)quantile(x,.75))
    t4 <- tapply(x,l,max)
    matplot(cbind(t0,t1,t2,t3,t4),
            type='l', 
            lty=c(rep(3,length(l2)), rep(2,length(l2)), 
                  rep(1,length(l2)), rep(2,length(l2)),
                  rep(3,length(l2)) ),
            col=1:length(l2),
            axes=F, ...)
    axis(1, 1:2, l1)
    axis(2)
    lim <- par("usr")
    legend(lim[1] + .05*(lim[2]-lim[1]), lim[4],
           l2, lwd=1, lty=1, col=1:length(l2) )
  }
  op <- par(mfrow=c(1,2))
  myplot3( x, list(y,z), ylab = "" )
  myplot3( x, list(z,y), ylab = "" )
  par(op)
dev.off()

png(file="g278.png", width=600, height=600)
  shaded.pie <- function (...) {
    pie(...)
    op <- par(new=T)
    a <- seq(0,2*pi,length=100)
    for (i in (256:64)/256) {
      r <- .8-.1*(1-i)
      polygon( .1+r*cos(a), -.2+r*sin(a), border=NA, col=rgb(i,i,i))
    }
    par(new=T)
    pie(...)
    par(op)
  }
  x <- rpois(10,5)
  x <- x[x>0]
  shaded.pie(x)
dev.off()
detach.everything()

png(file="g279.png", width=600, height=600)
  library(MASS)
  data(beav1)
  plot(beav1$temp ~ beav1$time)
dev.off()

png(file="g280.png", width=600, height=600)
  x <- beav1$time
  y <- beav1$temp
  o <- order(x)
  x <- x[o]
  y <- y[o]
  plot(y ~ x, 
       type = "l",
       xlab = "Time",
       ylab = "Temperature", 
       main = "The \"plot\" function, with type=\"l\"")
dev.off()

png(file="g281.png", width=600, height=600)
  plot(y ~ x, 
       type = "b",
       lwd = 3,
       xlim = c(0, 400),
       xlab = "Time",
       ylab = "Temperature", 
       main = "The \"plot\" function, with type=\"b\"")
dev.off()

png(file="g282.png", width=600, height=600)
  x <- as.matrix( EuStockMarkets[1:50,] )
  matplot(x,                 # By default: not lines, 
          main = "matplot",  # but unconnected coloured numbers
          xlab = "",
          ylab = "")
dev.off()

png(file="g283.png", width=600, height=600)
  matplot(x, 
          type = "l",    # Lines -- but I am not happy 
          lty = 1,       # with the axes
          xlab = "",
          ylab = "",
          main = "matplot")
dev.off()

png(file="g284.png", width=600, height=600)
  x <- as.matrix( EuStockMarkets )
  matplot(time(EuStockMarkets), 
          x, 
          log = "y",
          type = 'l', 
          lty = 1, 
          ylab = "Closing price", 
          xlab = "Date", 
          main = "matplot",
          axes = FALSE)
  axis(1)
  axis(2)
  box()
dev.off()

png(file="g285.png", width=600, height=600)
  pairs(longley)
dev.off()

png(file="g286.png", width=600, height=600)
  pairs(longley, 
        gap=0,
        diag.panel = function (x, ...) {
          par(new = TRUE)
          hist(x, 
               col = "light blue", 
               probability = TRUE, 
               axes = FALSE, 
               main = "")
          lines(density(x), 
                col = "red", 
                lwd = 3)
          rug(x)
        })
dev.off()

png(file="g287.png", width=600, height=200)
  stripchart(longley$Unemployed)  
dev.off()

png(file="g288.png", width=600, height=600)
  hist(longley$Unemployed)
dev.off()

png(file="g289.png", width=600, height=600)
  hist(longley$Unemployed,
       probability = TRUE,    # Change the vertical units, 
                              # to overlay a density estimation
       col = "light blue")
  lines(density(longley$Unemployed),
        col = "red",
        lwd = 3)
dev.off()

png(file="g290.png", width=200, height=600)
  boxplot(longley$Unemployed)
dev.off()

png(file="g291.png", width=600, height=200)
  boxplot(longley$Unemployed,
          horizontal = TRUE,
          col = "pink",
          main = "Box-and-whiskers plot (boxplot)")
dev.off()

png(file="g292.png", width=600, height=600)
  data(InsectSprays)
  boxplot(count ~ spray, 
          data = InsectSprays, 
          col = "pink",
          xlab = "Spray", 
          ylab = "Count",
          main = "Insect sprays")
dev.off()

png(file="g293.png", width=600, height=600)
  boxplot(count ~ spray, 
          data = InsectSprays, 
          col = "pink", 
          horizontal = TRUE, 
          las = 1,            # Horizontal labels
          xlab = "Count", 
          ylab = "Spray",
          main = "Insect sprays")
dev.off()

png(file="g294.png", width=600, height=600)
  N <- 50
  x <- seq(-1, 1, length=N)
  y <- seq(-1, 1, length=N)
  xx <- matrix(x, nr=N, nc=N)
  yy <- matrix(y, nr=N, nc=N, byrow=TRUE)
  z <- 1 / (1 + xx^2 + (yy + .2 * sin(10*yy))^2)
  contour(x, y, z,
          main = "Contour plot")
dev.off()

png(file="g295.png", width=600, height=600)
  image(z)
dev.off()

png(file="g296.png", width=600, height=600)
  image(x, y, z,
        xlab = "",
        ylab = "")
  contour(x, y, z, lwd=3, add=TRUE)
dev.off()

png(file="g297.png", width=600, height=600)
  persp(z)
dev.off()

png(file="g298.png", width=600, height=600)
  op <- par(mar=c(0,0,3,0)+.1)
  persp(x, y, z, 
        theta = 45, phi = 30, 
        shade = .5, 
        col = rainbow(N), 
        border = "green",
        main = "perspective plot, theta=45, phi=30")
  par(op)
dev.off()

png(file="g299.png", width=600, height=600)
  # From the manual: the sinc function
  x <- seq(-10, 10, length= 30)
  y <- x
  f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
  z <- outer(x, y, f)
  z[is.na(z)] <- 1
  op <- par(bg = "white", mar=c(0,2,3,0)+.1)
  persp(x, y, z, 
        theta = 30, phi = 30, 
        expand = 0.5, 
        col = "lightblue",
        ltheta = 120, 
        shade = 0.75, 
        ticktype = "detailed",
        xlab = "X", ylab = "Y", zlab = "Sinc(r)",
        main = "The sinc function"
  )
  par(op)
dev.off()

png(file="g300.png", width=600, height=600)
  n <- 100
  x <- rnorm(n)
  y <- 1 - x^2 + .3*rnorm(n)
  plot(y ~ x, 
       xlab = 'X axis', 
       ylab = "Y axis", 
       main = "Title")
dev.off()

png(file="g301.png", width=600, height=600)
  plot(y ~ x, 
       xlab = "", 
       ylab = "", 
       main = "")
  title(main = "Title", 
        xlab = "X axis", 
        ylab = "Y axis")
dev.off()

png(file="g302.png", width=600, height=600)
  set.seed(1)
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0,1))
  box()
  N <- 50
  text(  
    runif(N), runif(N), 
    sample(    # Random words...
      scan("/usr/share/dict/cracklib-small", character(0)), 
      N
    ) 
  )
dev.off()

png(file="g303.png", width=600, height=600)
  N <- 200
  x <- runif(N, -4, 4)
  y <- sin(x) + .5 * rnorm(N)
  plot(x, y, 
       xlab = "", ylab = "", 
       main = paste("The \"mtext\" function",
                    paste(rep(" ", 60), collapse="")))
  mtext("Line 0", 3, line=0)
  mtext("Line 1", 3, line=1)
  mtext("Line 2", 3, line=2)
  mtext("Line 3", 3, line=3)
dev.off()

png(file="g304.png", width=600, height=600)
  N <- 200
  x <- runif(N, -4, 4)
  y <- sin(x) + .5 * rnorm(N)
  plot(x, y, xlab="", ylab="", main="")
  mtext("Subtitle", 3, line=.8)
  mtext("Title",    3, line=2, cex=1.5)
  mtext("X axis", 1,          line=2.5, cex=1.5)
  mtext("X axis subtitle", 1, line=3.7)
dev.off()

png(file="g305.png", width=600, height=600)
  N  <- 200
  x  <- seq(-4,4, length=N)
  y1 <- sin(x)
  y2 <- cos(x)
  op <- par(mar=c(5,4,4,4)) # Add some space in the right margin
                            # The default is c(5,4,4,2) + .1
  xlim <- range(x)
  ylim <- c(-1.1, 1.1)
  plot(x, y1, col="blue", type="l", 
       xlim=xlim, ylim=ylim,
       axes=F, xlab="", ylab="", main="Title")
  axis(1)
  axis(2, col="blue")
  par(new=TRUE)
  plot(x, y2, col="red", type="l", 
       xlim=xlim, ylim=ylim,
       axes=F, xlab="", ylab="", main="")
  axis(4, col="red")
  mtext("First Y axis",  2, line=2, col="blue", cex=1.2)
  mtext("Second Y axis", 4, line=2, col="red",  cex=1.2)
dev.off()

png(file="g306.png", width=600, height=600)
  x <- seq(-5,5,length=200)
  y <- sqrt(1+x^2)
  plot(y~x, type='l',
       ylab=expression( sqrt(1+x^2) ))
  title(main=expression(
    "graph of the function f"(x) == sqrt(1+x^2)
  ))
dev.off()

png(file="g307.png", width=600, height=600)
  x <- seq(-5,5,length=200)
  op <- par(mfrow=c(2,2))
  for (i in 1:4) {
    y <- sqrt(i+x^2)
    plot(y ~ x, 
         type = 'l', 
         ylim = c(0,6),
         ylab = substitute( 
           expression( sqrt(i+x^2) ), 
           list(i=i) 
         ))
    title(main = substitute(
      "graph of the function f"(x) == sqrt(i+x^2),
      list(i=i)))
  }
  par(op)
dev.off()

png(file="g308.png", width=600, height=600)
  # From the manual
  plot(1:10, 1:10, main = "text(...) examples\n~~~~~~~~~~~~~~",
       sub = "R is GNU 漏, but not 庐 ...")
  mtext("芦ISO-accents禄: 卤 茅猫 酶脴 氓<脜 忙<脝", side=3)
  points(c(6,2), c(2,1), pch = 3, cex = 4, col = "red")
  text(6, 2, "the text is CENTERED around (x,y) = (6,2) by default",
       cex = .8)
  text(2, 1, "or Left/Bottom - JUSTIFIED at (2,1) by `adj = c(0,0)'",
       adj = c(0,0))
  text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
  text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)", cex = .75)
  text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
dev.off()

png(file="g309.png", width=600, height=600)
  # From the manual
  plot(1:9, type="n", axes=FALSE, frame=TRUE, ylab="",
       main= "example(Japanese)", xlab= "using Hershey fonts")
  par(cex=3)
  Vf <- c("serif", "plain")
  text(4, 2, "\\#J2438\\#J2421\\#J2451\\#J2473", vfont = Vf)
  text(4, 4, "\\#J2538\\#J2521\\#J2551\\#J2573", vfont = Vf)
  text(4, 6, "\\#J467c\\#J4b5c", vfont = Vf)
  text(4, 8, "Japan", vfont = Vf)
  par(cex=1)
  text(8, 2, "Hiragana")
  text(8, 4, "Katakana")
  text(8, 6, "Kanji")
  text(8, 8, "English")
dev.off()

png(file="g310.png", width=600, height=600)
  # Other example from the manual 
  # (it also contains katakana and kanji tables)
  make.table <- function(nr, nc) {
    opar <- par(mar=rep(0, 4), pty="s")
    plot(c(0, nc*(10%/%nc) + 1), c(0, -(nr + 1)),
         type="n", xlab="", ylab="", axes=FALSE)
    invisible(opar)
  }

  get.r <- function(i, nr)   i %% nr + 1
  get.c <- function(i, nr)   i %/% nr + 1
  Esc2 <- function(str)      paste("\\", str, sep="")

  draw.title <- function(title, nc)
    text((nc*(10%/%nc) + 1)/2, 0, title, font=2)

  draw.vf.cell <- function(typeface, fontindex, string, i, nr, raw.string=NULL) {
    r <- get.r(i, nr)
    c <- get.c(i, nr)
    x0 <- 2*(c - 1)
    if (is.null(raw.string)) raw.string <- Esc2(string)
    text(x0 + 1.1, -r, raw.string, col="grey")
    text(x0 + 2,   -r, string, vfont=c(typeface, fontindex))
    rect(x0 +  .5, -(r - .5), x0 + 2.5, -(r + .5), border="grey")
  }

  draw.vf.cell2 <- function(string, alt, i, nr) {
    r <- get.r(i, nr)
    c <- get.c(i, nr)
    x0 <- 3*(c - 1)
    text(x0 + 1.1, -r, Esc2(string <- Esc2(string)), col="grey")
    text(x0 + 2.2, -r, Esc2(Esc2(alt)), col="grey", cex=.6)
    text(x0 + 3,   -r, string, vfont=c("serif", "plain"))
    rect(x0 +  .5, -(r - .5), x0 + 3.5, -(r + .5), border="grey")
  }

  tf <- "serif"
  fi <- "plain"
  nr <- 25
  nc <- 4
  oldpar <- make.table(nr, nc)
  index <- 0
  digits <- c(0:9,"a","b","c","d","e","f")
  draw.title("Hiragana : \\\\#J24nn", nc)
  for (i in 2:7) {
    for (j in 1:16) {
      if (!((i == 2 && j == 1) || (i == 7 && j > 4))) {
        draw.vf.cell(tf, fi, paste("\\#J24", i, digits[j], sep=""),
                     index, nr)
        index <- index + 1
      }
    }
  }
dev.off()

png(file="g311.png", width=600, height=600)
  plot(runif(5), ylim=c(0,1), type='l')
  for (i in c('red', 'blue', 'green')) {
    lines( runif(5), col=i )
  }
  title(main="Lines in various colours")
dev.off()

png(file="g312.png", width=600, height=600)
  plot(runif(5), ylim=c(0,1), type='n')
  for (i in 5:1) {
    lines( runif(5), col=i, lwd=i )
  }
  title(main = "Varying the line thickness")
dev.off()

png(file="g313.png", width=600, height=800)
  op <- par(mfrow=c(3,2))
  plot(runif(5), type = 'p', 
       main = "plot type 'p' (points)")
  plot(runif(5), type = 'l', 
       main = "plot type 'l' (lines)")
  plot(runif(5), type = 'b', 
       main = "plot type 'b' (both points and lines)")
  plot(runif(5), type = 's', 
       main = "plot type 's' (stair steps)")
  plot(runif(5), type = 'h', 
       main = "plot type 'h' (histogram)")
  plot(runif(5), type = 'n', 
       main = "plot type 'n' (no plot)")
  par(op)
dev.off()

png(file="g314.png", width=600, height=800)
  op <- par(mfrow=c(3,2), mar=c(3,1,5,1))
  plot(runif(5), lty = 1,
       axes = FALSE, type = "l", lwd = 3, 
       main = "lty = 1 (default, solid)")
  plot(runif(5), lty = 2,
       axes = FALSE, type = "l", lwd = 3, 
       main = "lty = 2 (dashed)")
  plot(runif(5), lty = 3,
       axes = FALSE, type = "l", lwd = 3, 
       main = "lty = 3 (dotted)")
  plot(runif(5), lty = "dotdash",
       axes = FALSE, type = "l", lwd = 3, 
       main = "lty = 4 (dot, dash)")
  plot(runif(5), lty = "longdash",
       axes = FALSE, type = "l", lwd = 3, 
       main = "lty = 5 (longdash)")
  plot(runif(5), lty = "twodash",
       axes = FALSE, type = "l", lwd = 3, 
       main = "lty = 6 (twodash)")
  par(op)
dev.off()

png(file="g315.png", width=600, height=600)
  # You can also cook up your own line type
  # by providing the length of each segment and 
  # each space
  op <- par(mfrow=c(2,2), mar=c(3,1,5,1))
  for (lty in c("42", "14", "8222", "82624222")) { 
    plot(runif(5), lty = lty,
       axes = FALSE, type = "l", lwd = 3, 
       main = paste("lty =", lty))
  }
  par(op)
dev.off()

png(file="g316.png", width=600, height=600)
  op <- par(mar=c(1,1,4,1)+.1)
  plot(0,0, 
       xlim = c(1,5), ylim = c(-.5,4), 
       axes = F, 
       xlab = '', ylab = '',
       main = "Available symbols")
  for (i in 0:4) {
    for (j in 1:5) {
      n <- 5*i+j
      points(j, i, 
             pch = n, 
             cex = 3)
      text(j,i-.25, as.character(n))
    }
  }
  par(op)
dev.off()

png(file="g317.png", width=600, height=600)
  hist(longley$Unemployed, density=3, angle=45)
dev.off()

png(file="g318.png", width=600, height=600)
  op <- par(mfrow = c(2, 2))
  for (i in 1:4) 
    plot(runif(20), runif(20), 
         main=paste("random plot (",i,")",sep=''))
  par(op)
  mtext("Four plots, without enough room for this title", 
         side=3, font=2, cex=2, col='red')
dev.off()

png(file="g319.png", width=600, height=600)
  op <- par(mfrow = c(2, 2), 
            oma = c(0,0,3,0)   # Outer margins
           )
  for (i in 1:4) 
    plot(runif(20), runif(20), 
         main=paste("random plot (",i,")",sep=''))
  par(op)
  mtext("Four plots, with some room for this title", 
        side=3, line=1.5, font=2, cex=2, col='red')
dev.off()

png(file="g320.png", width=600, height=600)
  op <- par(mfrow = c(2, 2), 
            oma = c(0,0,3,0),
            mar = c(3,3,4,1) + .1     # Margins
           )
  for (i in 1:4) 
    plot(runif(20), runif(20), 
         xlab = "", ylab = "",
         main=paste("random plot (",i,")",sep=''))
  par(op)
  mtext("Title", 
        side=3, line=1.5, font=2, cex=2, col='red')
  par(op)
dev.off()

png(file="g321.png", width=600, height=600)
  n <- 20
  x <- rnorm(n)
  y <- x^2 - 1 + .3*rnorm(n)
  plot(y ~ x,
       main = "The \"fig\" graphic parameter")  
  op <- par()
  for (i in 2:10) {
    done <- FALSE
    while(!done) {
      a <- c( sort(runif(2,0,1)), 
              sort(runif(2,0,1)) )
      par(fig=a, new=T)
      r <- try(plot(runif(5), type='l', col=i))
      done <- !inherits(r, "try-error")
    }
  }
  par(op)
dev.off()

png(file="g322.png", width=600, height=600)
  n <- 1000
  x <- rt(n, df=10)
  hist( x, 
        col = "light blue",
        probability = "TRUE",
        ylim = c(0, 1.2*max(density(x)$y)))
  lines(density(x),
        col = "red",
        lwd = 3)
  op <- par(fig = c(.02,.4,.5,.98), 
            new = TRUE)
  qqnorm(x, 
         xlab = "", ylab = "", main = "",
         axes = FALSE)
  qqline(x, col = "red", lwd = 2)
  box(lwd=2)
  par(op)
dev.off()

png(file="g323.png", width=600, height=600)
  op <- par(oma = c(0,0,3,0))
  layout(matrix(c(1, 1, 1,
                  2, 3, 4,
                  2, 3, 4), nr = 3, byrow = TRUE))
  hist( rnorm(n), col = "light blue" )
  hist( rnorm(n), col = "light blue" )
  hist( rnorm(n), col = "light blue" )
  hist( rnorm(n), col = "light blue" )
  mtext("The \"layout\" function",
        side = 3, outer = TRUE, 
        font = 2, size = 1.2)
  par(op)
dev.off()

png(file="g324.png", width=600, height=600)
  random.plot <- function () {
    N <- 200
    f <- sample(list(rnorm, 
                     function (x) { rt(x, df=2) }, 
                     rlnorm, 
                     runif), 
                1) [[1]]
    x <- f(N)
    hist(x, col="light blue", main="", xlab="", ylab="", axes=F)
    axis(1)
  }
  op <- par(bg="white", mar=c(2.5,2,1,2))
  split.screen(c(2,1))
  split.screen(c(1,3), screen = 2)
  screen(1); random.plot()
  #screen(2); random.plot() # Screen 2 was split into three screens: 3, 4, 5
  screen(3); random.plot()
  screen(4); random.plot()
  screen(5); random.plot()
  close.screen(all=TRUE)
  par(op)
dev.off()

png(file="g325.png", width=600, height=600)
  plot(runif(5), runif(5), 
       xlim = c(0,1), ylim = c(0,1))
  points(runif(5), runif(5), 
         col = 'orange', pch = 16, cex = 3)
  lines(runif(5), runif(5), 
        col = 'red')
  segments(runif(5), runif(5), runif(5), runif(5), 
           col = 'blue')
  title(main = "Overlaying points, segments, lines...")
dev.off()

png(file="g326.png", width=600, height=600)
  my.col <- function (f, g, xmin, xmax, col, N=200,
                      xlab="", ylab="", main="") {
    x <- seq(xmin, xmax, length = N)
    fx <- f(x)
    gx <- g(x)
    plot(0, 0, type = 'n', 
         xlim = c(xmin,xmax),
         ylim = c( min(fx,gx), max(fx,gx) ),
         xlab = xlab, ylab = ylab, main = main)
    polygon( c(x,rev(x)), c(fx,rev(gx)), 
             col = 'red', border = 0 )
    lines(x, fx, lwd = 3)
    lines(x, gx, lwd = 3)
  }
  op <- par(mar=c(3,3,4,1)+.1)
  my.col( function(x) x^2, function(x) x^2+10*sin(x), 
          -6, 6,
          main = "The \"polygon\" function")
  par(op)
dev.off()

png(file="g327.png", width=600, height=600)
  # From the manual
  ch.col <- c("rainbow(n, start=.7, end=.1)", 
              "heat.colors(n)",
              "terrain.colors(n)", 
              "topo.colors(n)", 
              "cm.colors(n)")
  n <- 16
  nt <- length(ch.col)
  i <- 1:n
  j <- n/nt
  d <- j/6
  dy <- 2*d
  plot(i, i+d, 
       type="n", 
       yaxt="n", 
       ylab="", 
       main=paste("color palettes;  n=",n))
  for (k in 1:nt) {
    rect(i-.5, (k-1)*j+ dy, i+.4,  k*j, 
         col = eval(parse(text=ch.col[k])))
    text(2*j,  k * j +dy/4, ch.col[k])
  }
dev.off()

png(file="g328.png", width=600, height=600)
  x <- seq(-6,6,length=200)
  y <- sin(x)
  z <- cos(x)
  plot(y~x, type='l', lwd=3, 
       ylab='', xlab='angle', main="Trigonometric functions")
  abline(h=0,lty=3)
  abline(v=0,lty=3)
  lines(z~x, type='l', lwd=3, col='red')
  legend(-6,-1, yjust=0,
         c("Sine", "Cosine"),
         lwd=3, lty=1, col=c(par('fg'), 'red'),
        )
dev.off()

png(file="g329.png", width=600, height=600)
  plot(y~x, type='l', lwd=3, 
       ylab='', xlab='angle', main="Trigonometric functions")
  abline(h=0,lty=3)
  abline(v=0,lty=3)
  lines(z~x, type='l', lwd=3, col='red')
  legend("bottomleft", 
         c("Sine", "Cosine"),
         lwd=3, lty=1, col=c(par('fg'), 'red'),
        )
dev.off()

png(file="g330.png", width=600, height=600)
  plot(y~x, type='l', lwd=3, 
       ylab='', xlab='angle', main="Trigonometric functions")
  abline(h=0,lty=3)
  abline(v=0,lty=3)
  lines(z~x, type='l', lwd=3, col='red')
  legend("bottomleft", 
         c("Sine", "Cosine"),
         inset = c(.03, .03),
         lwd=3, lty=1, col=c(par('fg'), 'red'),
        )
dev.off()

png(file="g331.png", width=600, height=600)
  op <- par(no.readonly=TRUE)
  plot(y~x, type='l', lwd=3, 
       ylab='', xlab='angle', main="Trigonometric functions")
  abline(h=0,lty=3)
  abline(v=0,lty=3)
  lines(z~x, type='l', lwd=3, col='red')
  par(xpd=TRUE)  # Do not clip to the drawing area
  lambda <- .025
  legend(par("usr")[1], 
         (1 + lambda) * par("usr")[4] - lambda * par("usr")[3],
         c("Sine", "Cosine"),
         xjust = 0, yjust = 0,
         lwd=3, lty=1, col=c(par('fg'), 'red'),
        )
  par(op)
dev.off()

png(file="g332.png", width=600, height=600)
  library(grid)
  grid.show.viewport(viewport(x=0.6, y=0.6,
                     w=unit(1, "inches"), h=unit(1, "inches")))
dev.off()

png(file="g333.png", width=600, height=600)
  grid.show.layout(grid.layout(4,2,
                   heights=unit(rep(1, 4),
                                c("lines", "lines", "lines", "null")),
                   widths=unit(c(1, 1), "inches")))
dev.off()

png(file="g334.png", width=600, height=600)
  dessine <- function () {
    push.viewport(viewport(w = 0.9, h = 0.9, 
                           xscale=c(-.1,1.1), yscale=c(-.1,1.1)))
    grid.rect(gp=gpar(fill=rgb(.5,.5,0)))
    grid.points( runif(50), runif(50) )
    pop.viewport()
  }
  grid.newpage()
  grid.rect(gp=gpar(fill=rgb(.3,.3,.3)))
  push.viewport(viewport(layout=grid.layout(2, 2)))
  for (i in 1:2) {
    for (j in 1:2) {
      push.viewport(viewport(layout.pos.col=i,
                             layout.pos.row=j))
      dessine()
      pop.viewport()
    }
  }
  pop.viewport()
dev.off()

png(file="g335.png", width=600, height=600)
  grid.multipanel(vp=viewport(0.5, 0.5, 0.8, 0.8))
dev.off()

png(file="g336.png", width=600, height=600)
  do.it <- function (x=runif(100), y=runif(100), 
                     a=.9, b=.1, 
                     col1=rgb(0,.3,0), col2=rgb(1,1,0)) {
    xscale <- range(x) + c(-1,1)*.05
    yscale <- range(y) + c(-1,1)*.05
    grid.newpage()
    grid.rect(gp=gpar(fill=col1, col=col1))
    w1 <- a - b/2
    w2 <- 1 - a - b/2
    c1 <- b/3 + w1/2
    c2 <- a + b/6 + w2/2
    vp1 <- viewport(x=c1, y=c1, width=w1, height=w1,
                    xscale=xscale, yscale=yscale)
    push.viewport(vp1)
    grid.rect(gp=gpar(fill=col2, col=col2))
    grid.points(x,y)
    pop.viewport()
    vp2 <- viewport(x=c1, y=c2, width=w1, height=w2,
                    xscale=xscale, yscale=c(0,1))
    push.viewport(vp2)
    grid.rect(gp=gpar(fill=col2, col=col2))
    grid.points(x,rep(.5,length(x)))
    pop.viewport()
    vp3 <- viewport(x=c2, y=c1, width=w2, height=w1,
                    xscale=c(0,1), yscale=yscale)
    push.viewport(vp3)
    grid.rect(gp=gpar(fill=col2, col=col2))
    grid.points(rep(.5,length(y)),y)
    pop.viewport()
  }
  do.it()
dev.off()

png(file="g337.png", width=600, height=600)
  data(quakes)
  library(lattice)
  Depth <- equal.count(quakes$depth, number=8, overlap=.1)
  xyplot(lat ~ long | Depth, data = quakes)
dev.off()

png(file="g338.png", width=600, height=600)
  plot(lat ~ long, data=quakes)
dev.off()

png(file="g339.png", width=600, height=600)
  op <- par(mfrow=c(2,2))
  plot(lat ~ long, data=quakes)
  plot(lat ~ -depth, data=quakes)
  plot(-depth ~ long, data=quakes)
  par(op)
dev.off()

png(file="g340.png", width=600, height=600)
  library(mva)
  biplot(princomp(quakes[1:3]))
dev.off()

png(file="g341.png", width=600, height=600)
  pairs( princomp(quakes[1:3])$scores )
dev.off()

png(file="g342.png", width=600, height=600)
  library(scatterplot3d)
  scatterplot3d(quakes[,1:3],
                highlight.3d = TRUE,
                pch = 20)
dev.off()

png(file="g343.png", width=600, height=600)
  data(barley)
  barchart(yield ~ variety | year * site, data=barley)
dev.off()

png(file="g344.png", width=600, height=600)
  barchart(yield ~ variety | year * site, data = barley,
           ylab = "Barley Yield (bushels/acre)",
           scales = list(x = list(0, abbreviate = TRUE,
                         minlength = 5)))
dev.off()

png(file="g345.png", width=600, height=600)
  dotplot(yield ~ variety | year * site, data = barley)
dev.off()

png(file="g346.png", width=400, height=1000)
  dotplot(variety ~ yield | year * site, data = barley)
dev.off()

png(file="g347.png", width=600, height=600)
  library(nlme)
  data(bdf)
  d <- data.frame( iq=bdf$IQ.perf, sex=bdf$sex, den=bdf$denomina )
  d <- d[1:100,] 
  bwplot( ~ d$iq | d$sex + d$den )
dev.off()

png(file="g348.png", width=600, height=600)
  histogram( ~ d$iq | d$sex + d$den )
dev.off()

png(file="g349.png", width=600, height=600)
  densityplot( ~ d$iq | d$sex + d$den )
dev.off()

png(file="g350.png", width=600, height=600)
  d <- data.frame( x=bdf$aritPOST, y=bdf$sex, z=equal.count(bdf$langPRET) )
  bwplot( ~ x | y + z, data=d )
dev.off()

png(file="g351.png", width=600, height=600)
  histogram( ~ x | y + z, data=d )
dev.off()

png(file="g352.png", width=600, height=600)
  densityplot( ~ x | y + z, data=d )
dev.off()

png(file="g353.png", width=600, height=600)
  d <- data.frame( x= (bdf$IQ.perf>11), y=bdf$sex, z=bdf$denomina )
  d <- as.data.frame(table(d))
  barchart( Freq ~ x | y * z, data=d )
dev.off()

png(file="g354.png", width=600, height=600)
  n <- 200
  x <- rnorm(n)
  y <- x^3+rnorm(n)
  plot1 <- xyplot(y~x)
  plot2 <- bwplot(x)
  # Beware, the order is xmin, ymin, xmax, ymax
  print(plot1, position=c(0, .2, 1, 1),  more=T)
  print(plot2, position=c(0, 0,  1, .2), more=F)
dev.off()

png(file="g355.png", width=600, height=600)
  n <- 200
  x <- rnorm(n)
  y <- x^4+rnorm(n)
  k <- .7
  op <- par(mar=c(0,0,0,0))
  # Attention : l'ordre est xmin, xmax, ymin, ymax
  par(fig=c(0,k,0,k))
  plot(y~x)
  par(fig=c(0,k,k,1), new=T)
  boxplot(x, horizontal=T)  
  par(fig=c(k,1,0,k), new=T)
  boxplot(y, horizontal=F)  
  par(op)
dev.off()

png(file="g356.png", width=800, height=800)
  show.settings()
dev.off()

png(file="g357.png", width=600, height=600)
  y <- cars$dist
  x <- cars$speed
  vitesse <- shingle(x, co.intervals(x, number=6))
  histogram(~ x | vitesse, type = "density",
            panel = function(x, ...) {
              ps <- trellis.par.get('plot.symbol')
              nps <- ps
              nps$cex <- 1
              trellis.par.set('plot.symbol', nps)
              panel.histogram(x, ...)
              panel.densityplot(x, col = 'brown', lwd=3) 
              panel.xyplot(x = jitter(x), y = rep(0, length(x)), col='brown' )
              panel.mathdensity(dmath = dnorm,
                                args = list(mean=mean(x),sd=sd(x)),
                                lwd=3, lty=2, col='white')
              trellis.par.set('plot.symbol', ps)
            })
dev.off()

png(file="g358.png", width=600, height=600)
  data(sunspot.year)
  sunspot <- sunspot.year[20 + 1:37]
  xyplot(sunspot ~ 1:37 ,type = "l",
         scales = list(y = list(log = TRUE)),
         sub = "log scales")
dev.off()

png(file="g359.png", width=600, height=600)
  xyplot(sunspot ~ 1:37 ,type = "l", aspect="xy",
         scales = list(y = list(log = TRUE)),
         sub = "log scales")
dev.off()
detach.everything()

png(file="g360.png", width=600, height=600)
  data(USArrests)
  p <- prcomp(USArrests)
  biplot(p)
dev.off()

png(file="g361.png", width=600, height=600)
  plot(p)
dev.off()

png(file="g362.png", width=600, height=600)
  a <- seq(0,2*pi,length=100)
  plot( cos(a), sin(a), 
        type = 'l', lty = 3,
        xlab = 'comp 1', ylab = 'comp 2', 
        main = "Correlation circle")
  v <- t(p$rotation)[1:2,]
  arrows(0,0, v[1,], v[2,], col='red')
  text(v[1,], v[2,],colnames(v))
dev.off()

png(file="g363.png", width=600, height=600)
  # Copy-pasted with the help of the "deparse" command:
  #   cat( deparse(x), file='foobar')
  notes <- matrix( c(15, NA, 7, 15, 11, 7, 7, 8, 11, 11, 13,
  6, 14, 19, 9, 8, 6, NA, 7, 14, 11, 13, 16, 10, 18, 7, 7,
  NA, 11, NA, NA, 6, 15, 5, 11, 7, 3, NA, 3, 1, 10, 1, 1,
  18, 13, 2, 2, 0, 7, 9, 13, NA, 19, 0, 17, 8, 2, 9, 2, 5,
  12, 0, 8, 12, 8, 4, 8, 0, 5, 5.5, 1, 12, 4, 13, 5, 11, 6,
  0, 7, 8, 11, 9, 9, 9, 14, 8, 5, 8, 5, 5, 12, 6, 16.5,
  13.5, 15, 3, 10.5, 1.5, 10.5, 9, 15, 7.5, 12, 13.5, 4.5,
  13.5, 13.5, 6, 12, 7.5, 9, 6, 13.5, 13.5, 15, 13.5, 6, NA,
  13.5, 4.5, 14, NA, 14, 14, 14, 8, 16, NA, 6, 6, 12, NA, 7,
  15, 13, 17, 18, 5, 14, 17, 17, 13, NA, NA, 16, 14, 18, 13,
  17, 17, 8, 4, 16, 16, 16, 10, 15, 8, 10, 13, 12, 14, 8,
  19, 7, 7, 9, 8, 15, 16, 8, 7, 12, 5, 11, 17, 13, 13, 7,
  12, 15, 8, 17, 16, 16, 6, 7, 11, 15, 15, 19, 12, 15, 16,
  13, 19, 14, 4, 13, 13, 19, 11, 15, 7, 20, 16, 10, 12, 16,
  14, 0, 0, 11, 9, 4, 10, 0, 0, 5, 11, 12, 7, 12, 17, NA, 6,
  6, 9, 7, 0, 7, NA, 15, 3, 20, 11, 10, 13, 0, 0, 6, 1, 5,
  6, 5, 4, 2, 0, 8, 9, NA, 0, 11, 11, 0, 7, 0, NA, NA, 7, 0,
  NA, NA, 6, 9, 6, 4, 5, 4, 3 ), nrow=30)
  notes <- data.frame(notes)
  # These are not the real names
  row.names(notes) <- 
    c("Anouilh", "Balzac", "Camus", "Dostoievski",
    "Eschyle", "Fenelon", "Giraudoux", "Homer",
    "Ionesco", "Jarry", "Kant", "La Fontaine", "Marivaux",
    "Nerval", "Ossian", "Platon", "Quevedo", "Racine",
    "Shakespeare", "Terence", "Updike", "Voltaire",
    "Whitman", "X", "Yourcenar", "Zola", "27", "28", "29",
    "30")
  attr(notes, "names") <- c("C1", "DM1", "C2", "DS1", "DM2",
                            "C3", "DM3", "DM4", "DS2")
  notes <- as.matrix(notes)
  notes <- t(t(notes) - apply(notes, 2, mean, na.rm=T))
  # Get rid of NAs
  notes[ is.na(notes) ] <- 0
  # plots
  plot(princomp(notes))
dev.off()

png(file="g364.png", width=600, height=600)
  biplot(princomp(notes))
dev.off()

png(file="g365.png", width=600, height=600)
  pairs(princomp(notes)$scores, gap=0)
dev.off()

png(file="g366.png", width=600, height=600)
  pairs(princomp(notes)$scores[,1:3])
dev.off()

png(file="g367.png", width=600, height=600)
  p <- princomp(notes)
  pairs( rbind(p$scores, p$loadings)[,1:3], 
         col=c(rep(1,p$n.obs),rep(2, length(p$center))),
         pch=c(rep(1,p$n.obs),rep(3, length(p$center))),
       )
dev.off()

png(file="g368.png", width=600, height=600)
  library(lattice)
  splom(as.data.frame(
    princomp(notes)$scores[,1:3]
  ))
dev.off()

png(file="g369.png", width=600, height=600)
  my.acp <- function (x) {
    n <- dim(x)[1]
    p <- dim(x)[2]
    # Translation, to use linear algebra
    centre <- apply(x, 2, mean)
    x <- x - matrix(centre, nr=n, nc=p, byrow=T)
    # diagonalizations, base changes
    e1 <- eigen( t(x) %*% x, symmetric=T )
    e2 <- eigen( x %*% t(x), symmetric=T )
    variables <- t(e2$vectors) %*% x
    subjects <- t(e1$vectors) %*% t(x)
    # The vectors we want are the columns of the 
    # above matrices. To draw them, with the "pairs"
    # function, we have to transpose them.
    variables <- t(variables)
    subjects <- t(subjects)
    eigen.values <- e1$values
    # Plot
    plot( subjects[,1:2], 
          xlim=c( min(c(subjects[,1],-subjects[,1])), 
                  max(c(subjects[,1],-subjects[,1])) ),
          ylim=c( min(c(subjects[,2],-subjects[,2])), 
                  max(c(subjects[,2],-subjects[,2])) ),
          xlab='', ylab='', frame.plot=F )
    par(new=T)
    plot( variables[,1:2], col='red',
          xlim=c( min(c(variables[,1],-variables[,1])), 
                  max(c(variables[,1],-variables[,1])) ),
          ylim=c( min(c(variables[,2],-variables[,2])), 
                  max(c(variables[,2],-variables[,2])) ),
          axes=F, xlab='', ylab='', pch='.')
    axis(3, col='red')
    axis(4, col='red')
    arrows(0,0,variables[,1],variables[,2],col='red')
    # Return the data
    invisible(list(data=x, centre=centre, subjects=subjects, 
                   variables=variables, eigen.values=eigen.values))
  }

  n <- 20
  p <- 5
  x <- matrix( rnorm(p*n), nr=n, nc=p )
  my.acp(x)
  title(main="ACP by hand")
dev.off()

png(file="g370.png", width=600, height=600)
  biplot(princomp(x))
dev.off()

png(file="g371.png", width=600, height=600)
  b <- read.table('ling.txt')
  names(b) <- c(letters[1:26], 'language')
  a <- b[,1:26]
  a <- a/apply(a,1,sum)
  biplot(princomp(a))
dev.off()

png(file="g372.png", width=600, height=600)
  plot(hclust(dist(a)))
dev.off()

png(file="g373.png", width=600, height=600)
  kmeans.plot <- function (data, n=2, iter.max=10) {
    k <- kmeans(data,n,iter.max)
    p <- princomp(data)
    u <- p$loadings
    # The observations
    x <- (t(u) %*% t(data))[1:2,]
    x <- t(x)
    # The centers
    plot(x, col=k$cluster, pch=3, lwd=3)
    c <- (t(u) %*% t(k$center))[1:2,]
    c <- t(c)
    points(c, col = 1:n, pch=7, lwd=3)
    # A segment joining each observation to its group center
    for (i in 1:n) {
      for (j in (1:length(data[,1]))[k$cluster==i]) {
        segments( x[j,1], x[j,2], c[i,1], c[i,2], col=i )
      }
    }
    text( x[,1], x[,2], attr(x, "dimnames")[[1]] )
  }
  kmeans.plot(a,2)
dev.off()

png(file="g374.png", width=600, height=600)
  n <- 100
  v <- .1
  a <- rcauchy(n)
  b <- rcauchy(n)
  c <- rcauchy(n)
  d <- data.frame( x1 =  a+b+c+v*rcauchy(n),
                   x2 =  a+b-c+v*rcauchy(n),
                   x3 =  a-b+c+v*rcauchy(n),
                   x4 = -a+b+c+v*rcauchy(n),
                   x5 =  a-b-c+v*rcauchy(n),
                   x6 = -a+b-c+v*rcauchy(n) )
  biplot(princomp(d))
dev.off()

png(file="g375.png", width=600, height=600)
  rank.and.normalize.vector <- function (x) {
    x <- (rank(x)-.5)/length(x)
    x <- qnorm(x)
  }
  rank.and.normalize <- function (x) {
    if( is.vector(x) )
      return( rank.and.normalize.vector(x) )
    if( is.data.frame(x) ) {
      d <- NULL
      for (v in x) {
        if( is.null(d) )
          d <- data.frame( rank.and.normalize(v) )
        else 
          d <- data.frame(d, rank.and.normalize(v))
      }
      names(d) <- names(x)
      return(d)
    }
    stop("Data type not handled")
  }
  biplot(princomp(apply(d,2,rank.and.normalize)))
dev.off()

png(file="g376.png", width=600, height=600)
  pairs( princomp(d)$scores )
dev.off()

png(file="g377.png", width=600, height=600)
  pairs( princomp(apply(d,2,rank.and.normalize))$scores )
dev.off()

png(file="g378.png", width=600, height=600)
  N <- 1000
  X <- matrix(runif(2*N, -1, 1), nc=2)
  plot(X)
dev.off()

png(file="g379.png", width=600, height=600)
  M <- matrix(rnorm(4), nc=2)
  Y <- X %*% M
  plot(Y)
dev.off()

png(file="g380.png", width=600, height=600)
  plot(Y)
  p <- prcomp(Y)$rotation
  abline(0, p[2,1] / p[1,1], col="red", lwd=3)
  abline(0, -p[1,1] / p[2,1], col="red", lwd=3)
  abline(0, M[1,2]/M[1,1], col="blue", lwd=3)  
  abline(0, M[2,2]/M[2,1], col="blue", lwd=3)  
dev.off()

png(file="g381.png", width=600, height=600)
  op <- par(mfrow=c(2,2), mar=c(1,1,1,1))
  for (i in 1:4) {
    N <- 1000
    X <- matrix(runif(2*N, -1, 1), nc=2)
    M <- matrix(rnorm(4), nc=2)
    Y <- X %*% M
    plot(Y, xlab="", ylab="")
    p <- prcomp(Y)$rotation
    abline(0, p[2,1] / p[1,1], col="red", lwd=3)
    abline(0, -p[1,1] / p[2,1], col="red", lwd=3)
    abline(0, M[1,2]/M[1,1], col="blue", lwd=3)  
    abline(0, M[2,2]/M[2,1], col="blue", lwd=3)  
  }
  par(op)
dev.off()

png(file="g382.png", width=600, height=600)
  op <- par(mfrow=c(2,2))
  hist(X[,1], col="light blue")
  hist(X[,2], col="light blue")
  hist(Y[,1], col="light blue")
  hist(Y[,2], col="light blue")  
  par(op)
dev.off()

png(file="g383.png", width=600, height=600)
  library(e1071)
  r <- ica(Y,.1)
  plot(r$projection)
dev.off()

png(file="g384.png", width=600, height=600)
  library(mlica)
  ICA <- function (x,...) {
     prPCA <- PriorNormPCA(x);
     prNCP <- proposeNCP(prPCA,0.1);
     mlica(prNCP,...)
  }

  set.seed(1) # It sometimes crashes...
  N <- 1000
  X <- matrix(runif(2*N, -1, 1), nc=2)
  M <- matrix(rnorm(4), nc=2)
  Y <- X %*% M
  r <- ICA(Y)
  plot(r$S)
dev.off()

png(file="g385.png", width=600, height=600)
  n <- 100
  v <- .1
  # (Almost) planar data
  x <- rnorm(n)
  y <- x*x + v*rnorm(n)
  z <- v*rnorm(n)
  d <- cbind(x,y,z)
  # A rotation
  random.rotation.matrix <- function (n=3) {
    m <- NULL
    for (i in 1:n) {
      x <- rnorm(n)
      x <- x / sqrt(sum(x*x))
      y <- rep(0,n)
      if (i>1) for (j in 1:(i-1)) {
        y <- y + sum( x * m[,j] ) * m[,j]
      }
      x <- x - y
      x <- x / sqrt(sum(x*x))
      m <- cbind(m, x)
    }
    m
  }
  m <- random.rotation.matrix(3)
  d <- t( m %*% t(d) )
  pairs(d)
  title(main="The points lie in a plane")
dev.off()

png(file="g386.png", width=600, height=600)
  pairs(cmdscale(dist(d),3))
  title(main="MDS")
dev.off()

png(file="g387.png", width=600, height=600)
  pairs(princomp(d)$scores)
  title(main="Principal Component Analysis")
dev.off()

png(file="g388.png", width=600, height=600)
  # Data
  n <- 200  # Number of patients, number of columns
  k <- 10   # Dimension of the ambient space
  nb.points <- 5
  p <- matrix( 5*rnorm(nb.points*k), nr=k )
  barycentre <- function (x, n) {
    # Add number between the values of x in order to get a length n vector
    i <- seq(1,length(x)-.001,length=n)
    j <- floor(i)
    l <- i-j
    (1-l)*x[j] + l*x[j+1]
  }
  m <- apply(p, 1, barycentre, n)
  data.broken.line <- t(m)
  data.noisy.broken.line <- data.broken.line + rnorm(length(data.broken.line))
  library(splines)
  barycentre2 <- function (y,n) {
    m <- length(y)
    x <- 1:m
    r <- interpSpline(x,y)    
    #r <- lm( y ~ bs(x, knots=m) )
    predict(r, seq(1,m,length=n))$y
  }
  data.curve <- apply(p, 1, barycentre2, n)
  data.curve <- t(data.curve)
  data.noisy.curve <- data.curve + rnorm(length(data.curve))
  data.real <- read.table("Tla_z.txt", sep=",")
  r <- prcomp(t(data.real))
  data.real.3d <- r$x[,1:3]

  library(cluster)
  library(ape)
  mst.of.classification <- function (x, k=6, ...) {
    x <- t(x) 
    x <- t( t(x) - apply(x,2,mean) )
    r <- prcomp(x)
    y <- r$x
    u <- r$rotation
    r <- kmeans(y,k)
    z <- r$centers
    m <- mst(dist(z))
    plot(y[,1:2], ...)
    points(z[,1:2], col='red', pch=15)
    w <- which(m!=0)
    i <- as.vector(row(m))[w]
    j <- as.vector(col(m))[w]
    segments( z[i,1], z[i,2], z[j,1], z[j,2], col='red' )
  }
  set.seed(1)
  mst.of.classification(data.curve, 6)
dev.off()

png(file="g389.png", width=600, height=600)
  mst.of.classification(data.curve, 6)
dev.off()

png(file="g390.png", width=600, height=600)
  op <- par(mfrow=c(2,2),mar=.1+c(0,0,0,0))
  for (k in c(4,6,10,15)) {
    mst.of.classification(data.curve, k, axes=F)
    box()
  }
  par(op)
dev.off()

png(file="g391.png", width=600, height=600)
  op <- par(mfrow=c(2,2),mar=.1+c(0,0,0,0))
  for (k in c(4,6,10,15)) {
    mst.of.classification(data.noisy.curve, k, axes=F)
    box()
  }
  par(op)
dev.off()

png(file="g392.png", width=600, height=600)
  op <- par(mfrow=c(2,2),mar=.1+c(0,0,0,0))
  for (k in c(4,6,10,15)) {
    mst.of.classification(data.broken.line, k, axes=F)
    box()
  }
  par(op)
dev.off()

png(file="g393.png", width=600, height=600)
  op <- par(mfrow=c(2,2),mar=.1+c(0,0,0,0))
  for (k in c(4,6,10,15)) {
    mst.of.classification(data.noisy.broken.line, k, axes=F)
    box()
  }
  par(op)
dev.off()

png(file="g394.png", width=600, height=600)
  op <- par(mfrow=c(2,2),mar=.1+c(0,0,0,0))
  for (k in c(4,6,10,15)) {
    mst.of.classification(data.real, k, axes=F)
    box()
  }
  par(op)
dev.off()

png(file="g395.png", width=600, height=600)
  op <- par(mfrow=c(3,3),mar=.1+c(0,0,0,0))
  for (k in c(4:6)) {
    for (i in 1:3) {
      mst.of.classification(data.real, k, axes=F)
      box()
    }
  }
  par(op)
dev.off()

png(file="g396.png", width=600, height=600)
  op <- par(mfrow=c(3,3),mar=.1+c(0,0,0,0))
  for (k in c(7:9)) {
    for (i in 1:3) {
      mst.of.classification(data.real, k, axes=F)
      box()
    }
  }
  par(op)
dev.off()

png(file="g397.png", width=600, height=600)
  op <- par(mfrow=c(3,3),mar=.1+c(0,0,0,0))
  for (k in c(10:12)) {
    for (i in 1:3) {
      mst.of.classification(data.real, k, axes=F)
      box()
    }
  }
  par(op)
dev.off()

png(file="g398.png", width=600, height=600)
  op <- par(mfrow=c(3,5),mar=.1+c(0,0,0,0))
  for (k in c(13:15)) {
    for (i in 1:3) {
      mst.of.classification(data.real, k, axes=F)
      box()
    }
  }
  par(op)
dev.off()

png(file="g399.png", width=600, height=600)
  library(ape)
  my.plot.mst <- function (d) {
    r <- mst(dist(t(d)))
    d <- prcomp(t(d))$x[,1:2]
    plot(d)
    n <- dim(r)[1]
    w <- which(r!=0)
    i <- as.vector(row(r))[w]
    j <- as.vector(col(r))[w]
    segments( d[i,1], d[i,2], d[j,1], d[j,2], col='red' )
  }
  my.plot.mst(data.broken.line)
dev.off()

png(file="g400.png", width=600, height=600)
  my.plot.mst(data.noisy.broken.line)
dev.off()

png(file="g401.png", width=600, height=600)
  my.plot.mst(data.curve)
dev.off()

png(file="g402.png", width=600, height=600)
  my.plot.mst(data.noisy.curve)
dev.off()

png(file="g403.png", width=600, height=600)
  my.plot.mst(data.real)
dev.off()

png(file="g404.png", width=600, height=600)
  my.plot.mst(t(data.real.3d))
dev.off()

png(file="g405.png", width=600, height=600)
  # Gives the list of the oaths from the branching nodes to the leaves
  chemins.vers.les.feuilles <- function (G) {
    nodes <- which(apply(G,2,sum)>2)
    leaves <- which(apply(G,2,sum)==1)
    res <- list()
    for (a in nodes) {
      for (b in which(G[a,]>0)) {
        if (! b %in% nodes) {
          res <- append(res,list(c(a,b)))
        }
      }
    }
    chemins.vers.les.feuilles.suite(G, nodes, leaves, res)
  }
  # Last coordinate of a vector
  end1 <- function (x) {
    n <- length(x)
    x[n]
  }
  # Last two coordinates of a vector
  end2 <- function (x) {
    n <- length(x)
    x[c(n-1,n)]
  }
  chemins.vers.les.feuilles.suite <- function (G, nodes, leaves, res) {
    new <- list()
    done <- T
    for (ch in res) {
      if( end1(ch) %in% nodes ) {
        # Pass
      } else if ( end1(ch) %in% leaves ) {
        new <- append(new, list(ch))
      } else {
        done <- F
        a <- end2(ch)[1]
        b <- end2(ch)[2]
        for (x in which(G[b,]>0)) {
          if( x != a ){
            new <- append(new, list(c( ch, x )))
          }
        }
      }
    }
    if(done) {
      return(new)
    } else {
      return(chemins.vers.les.feuilles.suite(G,nodes,leaves,new))
    }
  } 

  G <- matrix(c(0,1,0,0, 1,0,1,1, 0,1,0,0, 0,1,0,0), nr=4)
  chemins.vers.les.feuilles(G)

  # Compute the length of a path
  longueur.chemin <- function (chemin, d) {
    d <- as.matrix(d)
    n <- length(chemin)
    i <- chemin[ 1:(n-1) ]
    j <- chemin[ 2:n ]
    if (n==2) {
      d[i,j]
    } else {
      sum(diag(d[i,][,j]))
    }
  }

  G <- matrix(c(0,1,0,0, 1,0,1,1, 0,1,0,0, 0,1,0,0), nr=4)
  d <- matrix(c(0,2,4,3, 2,0,2,1, 4,2,0,3, 3,1,3,0), nr=4)
  chemins <- chemins.vers.les.feuilles(G)
  chemins
  l <- sapply(chemins, longueur.chemin, d)
  l
  stopifnot( l == c(2,2,1) )

  elague <- function (G0, d0) {
    d0 <- as.matrix(d0)
    G <- G0
    d <- d0
    res <- 1:dim(d)[1]
    chemins <- chemins.vers.les.feuilles(G)
    while (length(chemins)>0) {
      longueurs <- sapply(chemins, longueur.chemin, d)
      # Number of the shortest path
      i <- which( longueurs == min(longueurs) )[1]
      cat(paste("Removing", paste(res[chemins[[i]]],collapse=' '), "length =", longueurs[i],"\n"))
      # Nodes to remove
      j <- chemins[[i]] [-1]
      res <- res[-j]
      G <- G[-j,][,-j]
      d <- d[-j,][,-j]
      cat(paste("Removing", paste(j), "\n" ))
      cat(paste("", paste(res, collapse=' '), "\n"))
      chemins <- chemins.vers.les.feuilles(G)      
    }
    res
  }

  library(ape)
  my.plot.mst <- function (x0) {
    cat("Plotting the points\n")
    x <- prcomp(t(x0))$x[,1:2]
    plot(x)
    cat("Computing the distance matrix\n")
    d <- dist(t(x0))
    cat("Computing the MST (Minimum Spanning Tree)\n")
    G <- mst(d)
    cat("Plotting the MST\n")
    n <- dim(G)[1]
    w <- which(G!=0)
    i <- as.vector(row(G))[w]
    j <- as.vector(col(G))[w]
    segments( x[i,1], x[i,2], x[j,1], x[j,2], col='red' )
    cat("Pruning the tree\n")
    k <- elague(G,d)
    cat("Plotting the pruned tree\n")
    x <- x[k,]
    G <- G[k,][,k]
    n <- dim(G)[1]
    w <- which(G!=0)
    i <- as.vector(row(G))[w]
    j <- as.vector(col(G))[w]
    segments( x[i,1], x[i,2], x[j,1], x[j,2], col='red', lwd=3 )        
  }

  my.plot.mst(data.noisy.broken.line)
dev.off()

png(file="g406.png", width=600, height=600)
  my.plot.mst(data.noisy.curve)
dev.off()

png(file="g407.png", width=600, height=600)
  my.plot.mst(data.real)
dev.off()

png(file="g408.png", width=600, height=600)
  my.plot.mst(t(data.real.3d))
dev.off()

png(file="g409.png", width=600, height=600)
  # k: each point is linked to its k nearest neighbors
  # eps: each point is linked to all its neighbors within a radius eps

  isomap.incidence.matrix <- function (d, eps=NA, k=NA) {
    stopifnot(xor( is.na(eps), is.na(k) ))
    d <- as.matrix(d)
    if(!is.na(eps)) {
      im <- d <= eps
    } else {
      im <- apply(d,1,rank) <= k+1
      diag(im) <- F
    }
    im | t(im)
  }
  plot.graph <- function (im,x,y=NULL, ...) {
    if(is.null(y)) {
      y <- x[,2]
      x <- x[,1]
    }
    plot(x,y, ...)
    k <- which(  as.vector(im)  )
    i <- as.vector(col(im))[ k ]
    j <- as.vector(row(im))[ k ]
    segments( x[i], y[i], x[j], y[j], col='red' )
  }

  d <- dist(t(data.noisy.curve))
  r <- princomp(t(data.noisy.curve))
  x <- r$scores[,1]
  y <- r$scores[,2]

  plot.graph(isomap.incidence.matrix(d, k=5), x, y)
dev.off()

png(file="g410.png", width=600, height=600)
  plot.graph(isomap.incidence.matrix(d, eps=quantile(as.vector(d), .05)), 
             x, y)
dev.off()

png(file="g411.png", width=600, height=600)
  isomap.incidence.matrix <- function (d, eps=NA, k=NA) {
    stopifnot(xor( is.na(eps), is.na(k) ))
    d <- as.matrix(d)
    if(!is.na(eps)) {
      im <- d <= eps
    } else {
      im <- apply(d,1,rank) <= k+1
      diag(im) <- F
    }
    im | t(im) | mst(d)
  }
  plot.graph(isomap.incidence.matrix(d, eps=quantile(as.vector(d), .05)), 
             x, y)
dev.off()

png(file="g412.png", width=600, height=600)
  inf <- function (x,y) { ifelse(x<y,x,y) }
  isomap.distance <- function (im, d) {
    d <- as.matrix(d)
    n <- dim(d)[1]
    dd <- ifelse(im, d, Inf)
    for (k in 1:n) {
      dd <- inf(dd, matrix(dd[,k],nr=n,nc=n) + matrix(dd[k,],nr=n,nc=n,byrow=T))
    }
    dd
  }

  isomap <- function (x, d=dist(x), eps=NA, k=NA) {
    im <- isomap.incidence.matrix(d, eps, k)
    dd <- isomap.distance(im,d)
    r <- list(x,d,incidence.matrix=im,distance=dd)
    class(r) <- "isomap"
    r    
  }

  r <- isomap(t(data.noisy.curve), k=5)
  xy <- cmdscale(r$distance,2)   # long: around 30 seconds
  plot.graph(r$incidence.matrix, xy)
dev.off()

png(file="g413.png", width=600, height=600)
  plot.graph(r$incidence.matrix, xy, ylim=range(xy))
dev.off()

png(file="g414.png", width=600, height=600)
  library(pixmap)
  x <- read.pnm("photo1.ppm")
  d <- cbind( as.vector(x@red),
              as.vector(x@green),
              as.vector(x@blue) )
  m <- apply(d,2,mean)
  d <- t(  t(d) - m )
  s <- apply(d,2,sd)
  d <- t(  t(d) / s )
  library(som)
  r1 <- som(d,5,5)
  plot(r1)
dev.off()

png(file="g415.png", width=600, height=600)
  x <- r1$code.sum$x
  y <- r1$code.sum$y
  n <- r1$code.sum$nobs
  co <- r1$code   # Is it in the same order that x, y and n?
  co <- t( t(co) * s + m )
  plot(x, y, 
       pch=15,
       cex=5,      
       col=rgb(co[,1], co[,2], co[,3])
      )
dev.off()

png(file="g416.png", width=600, height=600)
  x <- r1$code.sum$x
  y <- r1$code.sum$y
  n <- r1$code.sum$nobs
  co <- r1$code   # Is it in the same order that x, y and n?
  co <- t( t(co) * s + m )
  plot(x, y, 
       pch=15,
       cex=5*n/max(n),      
       col=rgb(co[,1], co[,2], co[,3])
      )
dev.off()

png(file="g417.png", width=600, height=600)
  library(class)
  r2 <- SOM(d)
  plot(r2)
dev.off()

png(file="g418.png", width=600, height=600)
  x <- r2$grid$pts[,1]
  y <- r2$grid$pts[,2]
  n <- 1   # Where???
  co <- r2$codes
  co <- t( t(co) * s + m )
  plot(x, y, 
       pch=15,
       cex=5*n/max(n),
       col=rgb(co[,1], co[,2], co[,3])
      )
dev.off()

png(file="g419.png", width=600, height=600)
  op <- par(mfrow=c(2,2))
  for (i in 1:4) {
    r2 <- SOM(d) 
    plot(r2)
  }
  par(o