#---------------------------------------------------------
# File:   MIT18_05S22_oldfaithful.r
# Author: Jeremy Orloff

# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------
# This code computes 3 bootstrap statistics for the old faithful data.
# 1. A 90% percentile CI for the median
# 2. A 90% percentile CI for the mean
# 3. P(|xbar - mu| > 5.
       
print('Old Faithful Data')
       
# Number of bootstrap samples to generate
nboot = 1000

#Make sure the working directory contains oldfaithful.txt
ofl = read.table('MIT18_05S22_oldfaithful-data.txt');
ofl_data = ofl[[1]]
n = length(ofl_data) 
hist(ofl_data, nclass=20)       

#Sample median and mean
ofl_median = median(ofl_data)
ofl_mean = mean(ofl_data)

# COMMENT: Could generate one sample at a time by doing everything in the for loop.

#Generate nboot empirical bootstrap trials
# each trial consists of n samples from the data
x = sample(ofl_data, n*nboot, replace=TRUE)
boot_data = matrix(x, nrow=n, ncol=nboot)

#Compute the medians and means of the bootstrap trials
boot_medians = rep(0, nboot)
boot_means = rep(0, nboot)
for (j in 1:nboot)
{
  boot_medians[j] = median(boot_data[,j])
  boot_means[j] = mean(boot_data[,j])  
}
hist(boot_medians, nclass=20)       
hist(boot_means, nclass=20)       

# Compute the 0.05 and 0.95 quantiles for the bootstrap medians
# Quantile has many methods of interpolating to compute the quantiles,
# e.g for median, quantil(1:4, 0.5) = 2.5.
# We just go with the default method
q_median_05 = quantile(boot_medians, 0.05);
q_median_95 = quantile(boot_medians, 0.95);

q_mean_05 = quantile(boot_means, 0.05);
q_mean_95 = quantile(boot_means, 0.95);

# Compute the 90% percentile bootstrap confidence intervals
CI_median = c(q_median_05, q_median_95)
CI_mean = c(q_mean_05, q_mean_95)
       
# Compute the fraction of the bootstrap samples where 
#the  |xbar_star - xbar|  > 5 
prob_diff_5 = sum(abs(boot_means - ofl_mean) > 5)/nboot 
     
s = sprintf("Mean = %.3f, median = %.3f", ofl_mean, ofl_median)
cat(s, '\n')
s = sprintf("CI_median: [%.3f, %.3f]", CI_median[1], CI_median[2])
cat(s, '\n')
s = sprintf("CI_mean: [%.3f, %.3f]", CI_mean[1], CI_mean[2])
cat(s, '\n')
s = sprintf("P(|xbar_star - xbaar| > 5) = %.3f", prob_diff_5)
cat(s, '\n')
