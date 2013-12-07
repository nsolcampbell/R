> ## generate linear regression relationship
> ## with homoskedastic variances
> x <- sin(1:100)
> y <- 1 + x + rnorm(100)
> ## model fit and HC3 covariance
> fm <- lm(y ~ x)
> vcovHC(fm)
            (Intercept)           x
(Intercept) 0.010809366 0.001209603
x           0.001209603 0.018353076
> coeftest(fm, vcov. = vcovHC)

t test of coefficients:

            Estimate Std. Error t value  Pr(>|t|)    
(Intercept)  1.01973    0.10397  9.8081 3.159e-16 ***
x            0.93992    0.13547  6.9381 4.313e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
To get the F test, look at function waldtest():

> waldtest(fm, vcov = vcovHC)
Wald test

Model 1: y ~ x
Model 2: y ~ 1
  Res.Df Df      F    Pr(>F)    
1     98                        
2     99 -1 48.137 4.313e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
You could always cook up a simple function to combine these two for you if you wanted the one-liner...

There are lots of examples in the Econometric Computing with HC and HAC Covariance Matrix Estimators vignette that comes with the sandwich package of linking lmtest and sandwich to do what you want.

Edit: A one-liner could be as simple as:

mySummary <- function(model, VCOV) {
    print(coeftest(model, vcov. = VCOV))
    print(waldtest(fm, vcov = VCOV))
}
Which we can use like this (on the examples from above):

> mySummary(fm, vcovHC)

t test of coefficients:

            Estimate Std. Error t value  Pr(>|t|)    
(Intercept)  1.01973    0.10397  9.8081 3.159e-16 ***
x            0.93992    0.13547  6.9381 4.313e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Wald test

Model 1: y ~ x
Model 2: y ~ 1
  Res.Df Df      F    Pr(>F)    
1     98                        
2     99 -1 48.137 4.313e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1