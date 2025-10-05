#---------------------------------------------------------
# File:   MIT18_05S22_CLEARALL.R
# Author: Jeremy Orloff 
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------
 # Run this to completely clean the environment
rm(list=ls())  #clear environment
cat("\014")    #clear RStudio console 
if (!is.null(dev.list())) {
  #dev.off()  #Clear plot window, reset plot pars
  dev.off(dev.list()["RStudioGD"])
}
 
