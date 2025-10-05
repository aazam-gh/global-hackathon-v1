#---------------------------------------------------------
# File:   MIT18_05S22_class24-empiricalbootstrap.r 
# Author: Jeremy Orloff
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------
# Class 24: empirical boostrap example

# Example. Empirical bootstrap confidence intervals for the mean.
# Data for the example in class24-prep-a

cat("Example. Empirical boostrap confidence intervals for the mean.",'\n')
x = c(30,37,36,43,42,43,43,46,41,42)
cat('Original sample:', x, '\n')
n = length(x)
alpha = 0.2

set.seed(1)  # for repeatability

# sample mean
xbar = mean(x)
cat('xbar:', xbar, '\n')

n_boot = 20
# Generate 20 bootstrap samples, i.e. an n x 20 array of random resamples from x.
tmp_data = sample(x, n*n_boot, replace=TRUE)
bootstrap_sample = matrix(tmp_data, nrow=n, ncol=n_boot)
cat('bootstrap samples\n')
for (j in 1:length(x)) {
  cat(bootstrap_sample[j,], '\n')
}
# Compute the means xbar*
xbar_star = colMeans(bootstrap_sample)

# Percentile method for 1-alpha CI for the mean
percentile_ci = quantile(xbar_star, c(alpha/2, 1-alpha/2))
s = sprintf('[%.2f, %.2f]', percentile_ci[1], percentile_ci[2])
cat(1-alpha, 'percentile confidence interval: ', s, '\n') 

# Basic method for 1-alpha CI
# Compute delta* for each bootstrap sample
delta_star = xbar_star - xbar

# Find the alpha/2 and 1-alpha/2 quantiles for delta_star
d = quantile(delta_star, c(alpha/2, 1-alpha/2))

# Calculate the 1-alpha basic confidence interval for the mean.
# Note that pivoting uses d[2], d[1]
basic_ci = xbar - c(d[2], d[1])
s = sprintf('[%.2f, %.2f]', basic_ci[1], basic_ci[2])
cat(1-alpha, 'basic confidence interval: ', s,'\n')

# Bootstrap t CI for the mean
# Idea: t = (xbar-mu)/(s/sqrt(n))
# So t_star = (xbar_star - xbar)/(s_star/sqrt(n))
# t1,t2 = alpha/2, 1-alpha/2 quantiles of t_star
# Apply bootstrap principle, pivot and undo studentization to get CI
# I.e. write  t1 < (xbar_star - xbar)/(s_star/sqrt(n))
# becomes  t1 < (xbar - mu)/(s/sqrt(n)) < t2
# So bootstrap CI is   xbar - t2*s/sqrt(n) < mu < xbar - t1*s/sqrt(n)
# Compute the sample variances for each bootstrap sample
#
# NOTE: The use of s/sqrt(n) is there because that is
# how we compute t-values. From a coding standpoint the
# factor of sqrt(n) in the definition of t_star is exactly
# canceled by the same factor in the definition of the
# confidence interval. Thus, we would  get identical results if
# we simply removed the sqrt(n) throughout the code.
# throughout this code, the results would be identical.
s = sd(x)
var_star = (colSums(bootstrap_sample^2) - n*xbar_star^2)/(n-1)
s_star = sqrt(var_star)
# Compute the t statistic for each bootstrap sample
t_star = (xbar_star - xbar)/(s_star/sqrt(n))
# Find the alpha/2 and 1-alpha/2 quantiles for t_star
t1 = quantile(t_star, alpha/2)
t2 = quantile(t_star, 1-alpha/2)
bootstrap_t_ci = xbar - c(t2,t1)*(s/sqrt(n))
s = sprintf('[%.2f, %.2f]', bootstrap_t_ci[1], bootstrap_t_ci[2])
cat(1-alpha, 'bootstrap t confidence interval: ', s,'\n')
# 
#---------------------------------------------------------#
# ALTERNATIVE: the quantile() function is sophisticated about choosing a quantile between two data points. We use the quantile function in the class notes. A less sophisticated approach is to pick the quantiles by sorting and choosing the index that corresponds to the desired quantiles. 

# Sort the results
sorted_xbar_star = sort(xbar_star)
sorted_delta_star = sort(delta_star)

# Find the 0.1 and 0.9 quantiles of xbar_star
q1_alt = sorted_xbar_star[2]
q9_alt = sorted_xbar_star[18]
# Find and print the 80\% percentile confidence interval for the mean
percentile_ci_alt = c(q1_alt, q9_alt)
s = sprintf('[%.1f, %.1f]', percentile_ci_alt[1], percentile_ci_alt[2])
cat('Alternative percentile confidence interval using indices: ', s,'\n')

# Find the 0.1 and 0.9 critical values of sorted_delta_star
d9_alt = sorted_delta_star[2]
d1_alt = sorted_delta_star[18]
# Find and print the 80\% basic confidence interval for the mean
basic_ci_alt = xbar - c(d1_alt, d9_alt)
s = sprintf('[%.2f, %.2f]', basic_ci_alt[1], basic_ci_alt[2])
cat('Alternative basic confidence interval using indices: ', s,'\n')
