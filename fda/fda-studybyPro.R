################################################################################
################################################################################
#                              LAB 1                                          #
################################################################################
################################################################################

# This lab is an introduction to the R package 'fda'. Here we will focus on
# defining functional objects, smoothing methods, manipulating functional
# data objects and some basis exploratory data analysis.

### Firstly, let's load up the library and some data.

library('fda')

# All of our examples will come from the Canadian Weather Data. Not all of these
# examples will therefore be particularly realistic. But they are there in order
# to demonstrate aspects of the code. Realism is retained as much as possible.

# We can load these data by

data(CanadianWeather)

# Temperature and precipitation are contained in the dailyAV element, and we'll
# extract them.

temp = CanadianWeather$dailyAv[,,1]
precip = CanadianWeather$dailyAv[,,2]

# We need corresponding time points. I'll put them half-way through a day. This
# is because the period is over 0:365 and we'd like the 365 data points to be
# about symmetric in that period.

daytime = (1:365)-0.5

# This is a bit fine for plotting purposes, so we'll also create a vector of
# points to use for plotting every 5 days

day5 = seq(0,365,5)


### Plot these data

# Note: I am assuming you will use only one plotting window and won't close it
# so I haven't insisted on re-setting the window as being two-pane below. We
# revert to a single pane graphical window just before the section on smoothing
# functions.

par(mfrow=c(2,1))
matplot(daytime,temp,type='l')
matplot(daytime,precip,type='l')

# We can also plot by region; Atlantic, Pacific, Central and North.

matplot(daytime,temp,type='l',col=as.factor(CanadianWeather$region))
matplot(daytime,precip,type='l',col=as.factor(CanadianWeather$region))

# But let's get on to fda.

################################################################################
###                          DEFINING A BASIS SYSTEM                        ####
################################################################################

# We'll always need the range

dayrng = c(0,365)

#### 1. Fourier Basis with 365 basis functions

fbasis = create.fourier.basis(dayrng,365)

# Plot some of these

plot(fbasis[1:5])
plot(fbasis[50])

# etc

# Let's try a simple linear regression of the first temperature record on the
# first, say, 20 basis functions

fb.values = eval.basis(day5,fbasis)

# the 74 by 365 matrix that results has rows as days, columns as bases

dim(fb.values)
plot(day5,fb.values[,1])
plot(day5,fb.values[,2])

# Extract the first temperature record

ex.temp = temp[,1]

# Run a linear regression on temperature with the first 5 basis functions. In
# this case I need to evaluate the basis at the observation times

Xmat = eval.basis(daytime,fbasis)
Xmat = Xmat[,1:5]  # First 5 basis functions

ex.temp.mod = lm(ex.temp~Xmat)

# Let's plot this; the fitted values returned by lm will represent the smooth
# well enough to plot.

plot(daytime,ex.temp)
lines(daytime,ex.temp.mod$fitted,col=2,lwd=2)

# We can also look at residuals

plot(daytime,ex.temp.mod$resid)

# There's some clear autocorrelation; more basis functions may be warranted.

###  EXERCISE: repeat the above with different numbers of basis functions (say
###  20, 50, 100, 200, 365. How many look like they give a reasonable smooth?

#### 2. B-spline bases with knots every 5 days

# First of all define a knot sequence; this will be the same as day5

knots = day5

# We'll use fourth-order B-splines

norder = 4

# this implies the number of basis functions

nbasis = length(knots) + norder - 2

# Now we can define the basis

bbasis = create.bspline.basis(dayrng,nbasis,norder,knots)
 
# If in doubt, we can obtain

bbasis$nbasis    # number of basis functions
bbasis$rangeval  # basis range

# Plotting these is a little crazy; but we can look at a few on a restricted
# range

plot(bbasis)
plot(bbasis[1:10],xlim=c(0,70))

# We can also look at the inner product of these

in.mat = inprod(bbasis,bbasis)

par(mfrow=c(1,1))
image(in.mat)

# and see that it is zero outside of a diagonal band; this can help computation
# a great deal.

### EXERCISE: try changing the order of the basis and observe how the width
### of the support of the basis changes and how its smoothness properties change.

### EXERCISE: obtain a least squares smooth of these data with the Bspline basis
### how does this compare with a least squares smooth using a Fourier basis with
### the same number of basis functions?

################################################################################
###                          SMOOTHING FUNCTIONS                            ####
################################################################################

#### 1. Lfd Objects

# Two common means of generating Lfd objects
# 1. int2Lfd -- just penalize some derivatives.

curv.Lfd = int2Lfd(2)

# 2. vec2Lfd -- a (constant) linear combination of derivatives; for technical
# reasons this also requires the range of the basis.

harmLfd = vec2Lfd(c(0,(2*pi/365)^2,0),rangeval=dayrng)

# looking inside these objects is not terribly enlightening.

#### 2. fdPar objects

# We'll concentrate on B-splines and second-derivative penalties.

# First, a value of lambda  (purposefully large so that we can distinguish a fit
# from data below).

lambda = 1e6

# Now we can define the fdPar object

curv.fdPar = fdPar(bbasis,curv.Lfd,lambda)

#### 3. Smoothing functions

# We're now in a position to smooth

tempSmooth1 = smooth.basis(daytime,temp,curv.fdPar)

# Let's look at the result

names(tempSmooth1)

# First of all, let's plot it

plot(tempSmooth1$fd)

# There is also a neat utility to go through each curve in turn and look at its
# fit to the data:

plotfit.fd(temp,daytime,tempSmooth1$fd)


# Let's examine some fit statistics

# degrees of freedom

tempSmooth1$df

# Just about equivalent to fitting 5 parameters

# We'll also look at GCV, this is given for each observation

tempSmooth1$gcv

# Let's change to a more realistic value of lambda

lambda = 1e1
curv.fdPar$lambda = lambda

tempSmooth = smooth.basis(daytime,temp,curv.fdPar)

# and repeat the previous steps

plotfit.fd(temp,daytime,tempSmooth$fd)
tempSmooth$df
tempSmooth$gcv

# Here the fit looks a lot better and the gcv values are much smaller.

#### 4. Choosing smoothing parameters

# We can search through a collection of smoothing parameters to try and find
# an optimal parameter.

# We will record the average gcv and choose lambda to be the minimum of these.

lambdas = 10^seq(-4,4,by=0.5)    # lambdas to look over

mean.gcv = rep(0,length(lambdas)) # store mean gcv


for(ilam in 1:length(lambdas)){
  # Set lambda
  curv.fdPari = curv.fdPar
  curv.fdPari$lambda = lambdas[ilam]

  # Smooth
  tempSmoothi = smooth.basis(daytime,temp,curv.fdPari)

  # Record average gcv
  mean.gcv[ilam] = mean(tempSmoothi$gcv)
}

# We can plot what we have

plot(lambdas,mean.gcv,type='b',log='x')

# Lets select the lowest of these and smooth

best = which.min(mean.gcv)
lambdabest = lambdas[best]

curv.fdPar$lambda = lambdabest
tempSmooth = smooth.basis(daytime,temp,curv.fdPar)

# And look at the same statistics

plotfit.fd(temp,daytime,tempSmooth$fd)
tempSmooth$df

# We'll also plot these

plot(tempSmooth)

### EXERCISE: try obtaining a smooth of the precipitation data

### EXERCISE: how much does the result change if the basis has a knot every day
### instead of every 5 days?


################################################################################
###      FUNCTIONAL DATA OBJECTS: MANIPULATION AND STATISTICS              ####
################################################################################

## Now that we have a functional data object we can manipulate them in various
# ways.  First let's extract the fd object

tempfd = tempSmooth$fd

# if we look at what's in this we see

names(tempfd)

# We see a basis, plus coefficient matrix

dim(tempfd$coefs)

# and an array giving names

tempfd$fdnames

# With lists giving names for time points, replicates and dimensions. Each list
# also has a name that can be used in plotting.  Apart from plotting functions,
# fdnames isn't used and you can generally ignore it.

# We can also create fd objects by adding a basis and a coefficient array. Let's
# make a random one, say

newcoefs = matrix(rgamma(nbasis*10,5,2),nbasis,10)
newfd = fd(newcoefs,bbasis)

# Notice that we haven't specified fdnames.

# The plotting command nicely draws these.

plot(newfd)

# Not that this looks very nice; we'll stick with the Canadian weather data.


#### 1. Manipulation

# We can do a number of things with these functions, treating them as data.
# These operations all result in new functional data objects, but we will plot
# them directly as an illustration.

# Subset

plot(tempfd[1:10])

# We can add them together; the 'lines' function also works with them

newfd = tempfd[1] + tempfd[2]
plot(newfd)
lines(tempfd[1],col=2)
lines(tempfd[2],col=4)

# We can also multiply

plot(tempfd[1]*tempfd[2])

# And obtain powers

plot(tempfd[1]^2)

# We can also obtain derivatives

plot(deriv.fd(tempfd))

# These are pretty wild because of the roughness of the resulting curves
# instead let's have a look at the over-smoothed data:

plot(deriv.fd(tempSmooth1$fd))

# We can also look at second derivatives

plot(deriv.fd(tempSmooth1$fd,2))

# Note that it is a property of B-splines of order m that the (m-2)th derivative
# is zero at the end of the interval.

#### 2. Summary statistics

# The obvious thing to look at is the mean

mtempfd = mean(tempfd)
plot(tempfd,col=4)
lines(mtempfd,lwd=2,col=2)

# We can also examine a variance

temp.varbifd = var.fd(tempfd)

# temp.varbifd is a bivariate functional data object -- meaning it takes values
# on a rectangle.

# To plot this, we need to evaluate it; here we'll use day5 -- 365 points is
# a bit overkill.

temp.var = eval.bifd(day5,day5,temp.varbifd)
contour(day5,day5,temp.var)

# Mostly high variance in the winter, low in summer. Let's have a look at
# correlation. In this case, evaluation arguments go in with the function call

temp.cor = cor.fd(day5,tempfd)
filled.contour(day5,day5,temp.cor)

# Here we see high correlation between Summer and Winter temperatures, but
# much less in the spring and fall (although spring to fall correlation is still
# high).

### EXERCISE: obtain these for the precipitation data and look at the covariance
### and correlation between temperature and precipitation. 

### EXERCISE: try repeating the above with a Fourier basis and the harmonic
### acceleration penalty. Does this make much difference?


################################################################################
###                  FUNCTIONAL PRINCIPAL COMPONENTS                      ####
################################################################################

#### 1. pca.fd

# We can conduct a fPCA through

tempPCA = pca.fd(tempfd,nharm=6)

# Here we can look at proportion of variance explained:

plot(tempPCA$varprop,type='b')

# Looks like we could have stopped at 3.

## Looking at the principal components:

plot(tempPCA$harmonics[1:3])

# 1 Looks like over-all temperature.
# 2 Looks like Summer vs Winter
# 3 Is Spring vs Fall.

## But let's plot these

plot(tempPCA,harm=1:3)

# Which gives a much better picture of what's going on.

##### 2. PCA and Smoothing

# The PCs above are fairly rough, we can also add a smoothing penalty (see
# the special topics slides).

pca.fdPar = fdPar(bbasis,curv.Lfd,1e4)

tempPCAsmooth = pca.fd(tempfd,nharm=6,harmfdPar=pca.fdPar)

# Let's plot the PCs

plot(tempPCAsmooth$harmonics[1:3])

# We can see that these are considerably smoother but still have pretty much
# the same interpretation.

plot(tempPCAsmooth)

##### 3. PCA and Reconstructing Data

# We can ask how well the PCs reconstruct the observed functions. We'll focus
# on the first observation and reconstructing using PC score.

# Let's extract the scores and PCs just to make the code easier to read

scores = tempPCAsmooth$scores
PCs = tempPCAsmooth$harmonics

# Firstly, just the mean + PC1

ex.temp.r1 = mtempfd + scores[1,1]*PCs[1]

# and plot these

plot(tempfd[1],ylim=c(-20,20))
lines(mtempfd,col=2)
lines(ex.temp.r1,col=3)

# Try adding the second PC

ex.temp.r2 = mtempfd + scores[1,1]*PCs[1] + scores[1,2]*PCs[2]
lines(ex.temp.r2,col=4)

# And the third

ex.temp.r3 = ex.temp.r2  + scores[1,3]*PCs[3]
lines(ex.temp.r3,col=6)

### THOUGHT EXERCISE: how would you use this to choose the level of smoothing
### in pca.fd by leave-one-curve-out cross validation?



##### 3. PCA of Multivariate Functions

# To look at two dimensions, we'll re-smooth the temperature data and look at
# it along with its derivative. To do that, let's consider a fourier basis
# and harmonic acceleration.

# First an fdPar object; we'll use 65 Fourier basis functions; more than enough
# but this will cut down the computational time. We'll also over-smooth so our
# derivatives don't look so wild.

fbasis =  create.fourier.basis(dayrng,65)

harm.fdPar = fdPar(fbasis,harmLfd,1e6)

# Do the smooth
tempSmooth2 = smooth.basis(daytime,temp,harm.fdPar)

# and take a derivative

temp.deriv = deriv.fd(tempSmooth2$fd)

# Now we need to duck under the hood to create a joine fd object for both
# temperature and precipitation at once.

# This basically means I need an fd object that stacks the coefficients for
# temperature and the coefficients for D temperature along a third dimension
# of a coefficient array.

Dtempcoefs = array(0,c(65,35,2))

Dtempcoefs[,,1] = tempSmooth2$fd$coefs
Dtempcoefs[,,2] = temp.deriv$coefs

# Let's also deal with the dimension names

Dtemp.fdnames = tempfd$fdnames
Dtemp.fdnames[[3]] = c('temperature','D temperature')

# Put it all together

Dtempfd = fd(Dtempcoefs,fbasis,Dtemp.fdnames)

# Now we can plot

par(mfrow=c(2,1))
plot(Dtempfd[,1])
plot(Dtempfd[,2])

# We can also look at covariance

Dtemp.varbifd = var.fd(Dtempfd)

# If we look at

Dtemp.var = eval.bifd(day5,day5,Dtemp.varbifd)

# We have a 74 x 74 x 1 x 3 array.

par(mfrow=c(1,1))

# First temperature
contour(day5,day5,Dtemp.var[,,1,1])

# Then d temperature
contour(day5,day5,Dtemp.var[,,1,3])

# Then their cross-product
contour(day5,day5,Dtemp.var[,,1,2])

### EXERCISE: experiment with cor.fd on this multidimensional fd object.

## Now let's look at the PCA

Dtemp.pca = pca.fd(Dtempfd,nharm=4)

# The PCs are now two dimensional

par(mfrow=c(2,1))
plot(Dtemp.pca$harmonics[,1])
plot(Dtemp.pca$harmonics[,2])

# We can plot these in the usual manner

plot(Dtemp.pca,harm=1:3)

# But we can also plot the whole cycle

par(mfrow=c(1,1))
plot(Dtemp.pca,harm=1:3,cycle=TRUE,xlab='temperature',ylab='D temperature')

# PC1 = over-all temperature and little variation in derivatives.
# PC2 = high summer-winter variation and also large variation in derivatives




################################################################################
################################################################################
#                              LAB 2                                          #
################################################################################
################################################################################

# This lab will focus on functional linear models using the fRegress function.
# We will again make use of the Canadian Weather Data. We'll briefly re-do
# the smooth here using Fourier bases:

data(CanadianWeather)

temp = CanadianWeather$dailyAv[,,1]
precip = CanadianWeather$dailyAv[,,2]

daytime = (1:365)-0.5
day5 = seq(0,365,5)

dayrng = c(0,365)

# Basis and parameter objects

fbasis =  create.fourier.basis(dayrng,65)
harmLfd = vec2Lfd(c(0,(2*pi/365)^2,0),rangeval=dayrng)
temp.fdPar = fdPar(fbasis,harmLfd,1e-2)

# And smooth

tempSmooth = smooth.basis(daytime,temp,temp.fdPar)

# And extract the functional data objects

tempfd = tempSmooth$fd



################################################################################
###                        SCALAR RESPONSE MODELS                          ####
################################################################################

## We examine predicting a scalar response from a functional covariate.

# In this case, we'll predict log annual precipitation from the temperature
# profile.  This is one of the most over-used examples in FDA; so get it out
# of your system here.

#### 1. Setup Data

# First we'll obtain log annual precipitation

annualprec = log10( apply(precip,2,mean))

# Now we need to set up a list of covariates.

xlist = list(len=2)

# First co-variate is just the intercept: a vector of ones

xlist[[1]] = rep(1,35)

# Second covariate is temperature

xlist[[2]] = tempfd


#### 2. fdPar objects for coeffients

# We also need a list of functional parameter objects to define the coefficient
# functions.

bwtlist = list(len=2)

# First is a constant basis and add it to the fdPar object

cbasis = create.constant.basis(dayrng)
bwtlist[[1]] = fdPar(cbasis)

# Now we need the coefficient of temperature, we'll use the same basis for it
# and add it as the second element in the list.

beta.fdPar = fdPar(fbasis,harmLfd,10^12.5)
bwtlist[[2]] = beta.fdPar

#### 3. fRegress and outcome

prec.model = fRegress(annualprec,xlist,bwtlist)

# Let's look at the outcome

names(prec.model)

# We can see the intercept as

prec.model$betaestlist[[1]]$fd$coef

# We can also plot the estimated regression function

plot(prec.model$betaestlist[[2]])


#### 4. Cross-validation

### We should look at selecting lambda. We'll do this with OCV

lambdas = 10^(seq(5,15,0.5))

ocvs = rep(0,length(lambdas))

for(ilam in 1:length(lambdas)){
  bwtlisti = bwtlist                # define temporary beta.fdPar and bwtlist
  beta.fdPari = beta.fdPar

  beta.fdPari$lambda = lambdas[ilam]  # update lambda
  bwtlisti[[2]] = beta.fdPari
 
  prec.modeli = fRegress(annualprec,xlist,bwtlisti)
 
  ocvs[ilam] = prec.modeli$OCV        # record ocv
}

plot(lambdas,ocvs,type='b',log='x')

# It looks like our original choice of 12.5 was about right.



#### 4. Statistics, Standard Errors and Tests

# Degrees of freedom

prec.model$df

# We'll plot y-by-yhat (in this case yhatfdobj is just a scalar despite
# its name).

yhat = prec.model$yhatfdobj

plot(yhat,annualprec)
abline(c(0,1))

# And we can caculate a residual variance

sigma = sum( (annualprec-yhat)^2 )/(35-prec.model$df)
sigma


# To obtain standard error estiamtes we can now call

sigmaE = sigma*diag(35)
prec.stderr = fRegress.stderr(prec.model,NULL,sigmaE)

# And we can obtain plots for beta from the estimated and standarderror

betahat = prec.model$betaestlist[[2]]
betastd = prec.stderr$betastderrlist[[2]]

plotbeta(betahat,betastd)

### EXERCISE: obtain a confidence interval for the intercept.

### THOUGHT EXERCISE: beta1(t) integrates to zero; why?

# Finally, we can run a permutation test on this model; this will take some
# time.

par(mfrow=c(1,1),ask=FALSE)
Fresult = Fperm.fd(annualprec,xlist,bwtlist)

# The histogram gives the permutation distribution. Dashed lines are the 95
# quantile and the solid line is the observed F value.

### EXERCISE: plot residuals against predicted values. This may not indicate
### poor fit if there is a non-linear relationship only with one part of the
### covariate function. Try a 3-dimensional plot putting time on the 'x' axis,
### the covariate value on the 'y' axis and residuals on the 'z' axis. The #
### library 'rgl' is particularly good for this.

### EXERCISE: how sensitive are these results to the amount of smoothing of the
### temperature process? Try lambda at 1e-6 and 1e2.

################################################################################
###              FUNCTIONAL PRINCIPAL COMPONENTS REGRESSION                ####
################################################################################

# Here we will continue the problem above, but we will tackle it from the
# perspective of functional Principal Components Regression.

#### 1. Obtaining an Estimate

# First we need to re-obtain fPCAs

tempPCA = pca.fd(tempfd,nharm=6)

# We'll continue to use the first three PCAs and extract their scores into
# a predictor matrix

Xmat = tempPCA$scores[,1:3]

# Now perform linear regression

prec.lm = lm(annualprec~Xmat)

# We can already obtain some summary statistics

summary(prec.lm)

# and try the same trick

plot(prec.lm$fitted,annualprec)
abline(c(0,1))

# Now we want to reconstruct beta. First we'll make the code easy to read by
# obtaining the PCs and the coefficients

Mcoefs = prec.lm$coef
PCs = tempPCA$harmonics[1:3]

# and put them together to get beta; remember, we still leave out the intercept.

beta = Mcoefs[2]*PCs[1] + Mcoefs[3]*PCs[2] + Mcoefs[4]*PCs[3]

# Now we can plot the result

plot(beta)

### EXERCISE: this is pretty rough -- try cross-validating the amount of
### smoothing in the PCA analysis based on its ability to predict log
### annual precipitation.

##### 2. Standard Errors

# We will do this manually. First we will use the usual variance

varbeta = sigma*solve(t(Xmat)%*%Xmat)

# Now we'll obtain the values of the PCs at a set of plotting points

PCvals = eval.fd(day5,PCs)

# The covariance for beta is then

PCbetacov = PCvals%*%varbeta%*%t(PCvals)

# We can actually have a look at the whole covariance surface

contour(day5,day5,PCbetacov)

# But largely we just want to extract the diagonal and then plot it.

# First we'll get values for beta

PCbetavals = eval.fd(day5,beta)

# Then standard errors

PCbetastd = sqrt(diag(PCbetacov))

# And we can form a plot

plot(day5,PCbetavals,type='l',lwd=2,ylim=c(-6e-4,1e-3))
lines(day5,PCbetavals+2*PCbetastd,type='l',lwd=2,lty=2)
lines(day5,PCbetavals-2*PCbetastd,type='l',lwd=2,lty=2)
abline(h=0)

# This is actually pretty similar to the fRegress version and will improve
# with smoothing the PCA.


################################################################################
###                      FUNCTIONAL RESPONSE MODELS                        ####
################################################################################

# We could predict total precipitation from temperature. How about the
# annual precipitation profile?

# We can also look at constant predictors -- see the weather demo for an ANOVA
# between different weather regions.

# First we'll create a smooth of the log precipitation

prec.fdPar = fdPar(fbasis,harmLfd,1e6)
precSmooth = smooth.basis(daytime,log(precip+0.5),prec.fdPar)
precfd = precSmooth$fd

# We can retain xlist from the scalar response model.


#### 1. fdPar objects and estimation

bwtlist2 = list(len=2)

# The intercept is now a functional parameter as well as beta 1.  Since this
# is an identifiable model without smoothing, we'll set the smoothing parameter
# very low.

beta.fdPar2 = fdPar(fbasis,harmLfd,1e-5)

bwtlist2[[1]] = beta.fdPar2
bwtlist2[[2]] = beta.fdPar2

# We can also call fRegress with this

prec.conc = fRegress(precfd,xlist,bwtlist2)

# Let's have a look at what we've got

par(mfrow=c(2,1))
plot(prec.conc$betaestlist[[1]])
plot(prec.conc$betaestlist[[2]])

# We can also look at a comparison between predicted and observed

yhatfd  = prec.conc$yhatfdobj$fd  # Fitted smooths.

plot(yhatfd)
plot(precfd)

# And compare observed with residuals

plot(precfd-yhatfd)
plot(precfd)

# Doesn't look like we really got very much.

#### 2. Confidence Intervals

## In order to obtain confidence intervals, we can include the smoothing of
## precipitation as a source of error. In order to do this, we need two things

y2cmap = precSmooth$y2cMap

# This is the matrix that goes from the observations to the coefficients.

# We now need a covariance matrix for the errors of the original observed
# precipitation from the functional linear model

Errmat = precip - eval.fd(daytime,yhatfd)

SigmaE2 = cov(t(Errmat))

# We can now run fRegress.stderr

conc.std = fRegress.stderr(prec.conc,y2cmap,SigmaE2)

# And plot the results

plotbeta(prec.conc$betaestlist,conc.std$betastderrlist)

# There really doesn't appear to be much going on.


### EXERCISE: try predicting precipitation instead of log precipitation -- does
### this make a difference?

### EXERCISE: what diagnostics could be done to check goodness of fit? Try
### plotting residuals. Try plotting residuals against predicted values
### (this should give you a series of lines, you'll need to evaluate and use
### 'matplot').    How could you check for dependence of precipitation on
### non-concurrent times?


#### 3. Permutation Tests and Cross Validation

### The next two functions can take a very long while to run

## Permutation test for fRegress

par(mfrow=c(1,1),ask=FALSE)
Fresult = Fperm.fd(precfd,xlist,bwtlist2)

# Here the dotted line gives the 95th percentile of the permutation distribution
# at each time t, the dashed line gives the 95th percentile of the permutation
# distribution of the maximum F value, and the solid line gives the observed
# F value at each time t. 


## Cross validated integrated squared error.

# This is a particularly long undertaking since the cross-validation is
# done manually. There are some matrix identities that can speed this up
# (still to be implemented). 

# In this case we will only look at the same lambda for each coefficient
# function

lambdas = 10^(c(-5,-2,1,3))
SISEs = rep(0,length(lambdas))

for(ilam  in 1:length(lambdas)){
    beta.fdPari = fdPar(fbasis,harmLfd,lambdas[ilam])  # Update lambda
    bwtlisti = list(beta.fdPari,beta.fdPari)

    CVres = fRegress.CV(precfd,xlist,bwtlisti)

    SISEs[ilam] = CVres$SSE.CV
   
    print(c(ilam,SISEs[ilam]))      # Just so we know where we are.
}

plot(lambdas,SISEs,type='b',log='x')

##### Further data sets to try playing with:

#  gait -- provides the gait data demo'd in class. Predict knee angle from
#  hip angle, or try predicting a derivative. The data are in 'gait'

#  Try predicting temperature from region in the weather data. You can find
#  a region indicator in "CanadianWeather$Region".  BUT, you will need to set
#  up an appropriate design matrix.
#
#  This is a functional version of an ANOVA.


################################################################################
###                          OTHER UTILITIES                              ####
################################################################################

# There are a number of other utilities in the fda library, that are worth
# checking out.

# 1. Constrained smooths, this includes
#
#  - smooth.pos  (positive smoothing -- see precipitation demo)
#  - smooth.mon  (monotone smoothing -- growth demo)
#  - density.fd  (log spline density estimation)
#
# These all have evaluation routines associated with them, too.

# 2. Registration, in particular
#
#  - landmarkreg  (landmark registration)
#  - register.fd  (automatic registration)
#
#  Good demos are the pinchforce data and the growth data.

# 3. Principal Differential Analysis
#
# The main engine for this is pda.fd which is just like a multivariate
# concurrent linear model with derivatives, but the signs can be different.
#
# See, in particular, the lip demo and the handwriting demo.
#
# plot.pda  provides some plotting  functions of coefficients
# eigen.pda  gives a stability analysis.

# 4. Unconstrained functional response models
#
# linmod will estimate a bivariate functional parameter for this case.
#
# See the weather data, or demo from Swedish mortality data.
#

###  Finally: code to produce all analyzes and figures in
#
# Ramsay & Silverman, "Functional Data Analysis"
#
# and
#
# Ramsay, Hooker and Graves, "Functional Data Analysis in R and Matlab",
#
# can be found in the "scripts" subdirectory of the fda directory in your R
# library.