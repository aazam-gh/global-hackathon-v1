#---------------------------------------------------------
# File:   MIT18_05S22_r_hist_tut.r
# Author: Jeremy Orloff
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

# Tutorial:
# Simple histogram examples ----
# R has the function hist() to make histograms of data

# Make up some data x
x = c(1,2,2,3,3,3,4,4,4,4)

#---------------------------------
# Default histograms
# The simplest thing to do is let R decide how to draw
# the histogram

# By default R makes a frequency histogram, i.e.
# the heights of the bars are the counts in each bin
hist(x, col='blue')

# If we want the area of each bar to represent the fraction of
# the data in the bin we need a density histogram. This is
# done by setting freq = FALSE
hist(x, col='cyan', freq=FALSE)
# Notice that the vertical axis is now labeled 'density'

#----------------------------------------
# Controlling the size and position of the bins
# R lets us control where the edges of each bin fall by
# using the 'breaks' parameter. Here we'll set each bin to
#  run between half integers.
brks = c(.5, 1.5, 2.5, 3.5, 4.5)
hist(x,breaks=brks, col="purple", freq=TRUE)

# We can use breaks with freq = FALSE
hist(x, breaks=brks, col="magenta", freq=FALSE)

# A different set of breaks
brks = c(0,1,2,3,4)
hist(x, breaks=brks, col="red", freq=TRUE)
hist(x, breaks=brks, col="orange", freq=FALSE)

brks = c(0,2,4)
hist(x, breaks=brks, col="blue", freq=FALSE)

brks = c(.25,.75,1.25,1.75,2.25,2.75,3.25,3.75,4.25)
hist(x, breaks=brks, col="cyan", freq=FALSE)

#----------------------------------
# Unequal bins
# We are allowed to use unequal width bins
brks = c(0,1,2,4)
# The frequency histogram generates a warning 
# because frequency is the count in each bin, so wider
# bins tend to be taller
hist(x, breaks=brks, col="red", freq=TRUE) #GENERATES WARNING

# Density diagrams are fine with unequal bins, because it is the 
# area of the bar that represents the fraction of the data in a bin
hist(x, breaks=brks, col="orange", freq=FALSE)

#-----------------------------------------
# An example with a lot of data
# The runif(n, 0, 1) functions generates random data from a uniform 
# distribution on [0,1]
x = runif(5000, 0, 1)

# We make a histogram with bins 0.05 wide
brks = seq(0, 1, 0.05)
hist(x, breaks=brks, col='orange')
# The histogram is pretty flat

# We can average together 2 independent copies
# and see what the  looks like
x1 = runif(5000, 0, 1)
x2 = runif(5000, 0, 1)
y = (x1+x2)/2
# Make a density histogram (we set the ylim to make sure it
# is big enough to accomodate the pdf we'll draw next)
hist(y, breaks=brks, col='cyan', freq=FALSE, ylim=c(0,2.2))

# We happen to know that the average y has range [0,1]
# and pdf 4t from 0 to 1/2 and 4-4t from 1/2 to 1
# So we plot the pdf on top of the histogram
a1 = seq(0, 0.5, 0.01)
b1 = 4*a1
a2 = seq(0.5, 1.0, 0.01)
b2 = 4-4*a2
lines(c(a1,a2), c(b1,b2), col='blue',lwd=2)
