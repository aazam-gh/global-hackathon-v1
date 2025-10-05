#---------------------------------------------------------
# File:   MIT18_05S22_class24-parametricbootstrap.r 
# Author: Jeremy Orloff
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------
# Class 24: parametric bootstrap example

cat('\nParametric bootstrap example\n')
# Given 300 data points with mean 2.
# Assume the data is exp(lambda)

# PROBLEM: Compute a 95% parametric bootstrap confidence interval for lambda
# We are given the number of data points and mean.
n = 300 
xbar = 2

# The MLE for lambda is 1/xbar
lambda_hat = 1/xbar

# Generate the bootstrap samples. 
# Each column is one bootstrap sample (of 300 resampled values) 
n_boot = 1000

# Here's the key difference with the empirical 
# bootstrap. We draw the bootstrap sample from Exponential(lambda_hat). 
x = rexp(n*n_boot, lambda_hat) 
bootstrap_sample = matrix(x, nrow=n, ncol=n_boot)

# Compute the bootstrap lambda_star
lambda_star = 1.0/colMeans(bootstrap_sample)

# Compute the differences
delta_star = lambda_star - lambda_hat

# Find the .05 and .95 quantile for delta_star
d = quantile(delta_star, c(.05,.95))

# Calculate the 95% confidence interval for lambda.
ci = lambda_hat - c(d[2], d[1])

# This line of code is just one way to format the output text. 
# sprintf is an old C function for doing this. R has many other
# ways to do the same thing. 
s = sprintf("Confidence interval for lambda: [%.3f, %.3f]", ci[1], ci[2]) 
cat(s, '\n')

