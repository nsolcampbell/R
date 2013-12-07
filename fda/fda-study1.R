#This is a test for the slides of Pro. Giles Hooker for a short course of Functional Data Analysis at the Glasgow University in 2010.

install.packages('fda')
require('fda')
setwd('d:\\dropbox\\Dropbox\\Apps\\R\\fda')

fbasis=create.fourier.basis(c(0,365),21)