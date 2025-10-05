#---------------------------------------------------------
# File:   MIT18_05S22_r_which_tut.r
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
# Using the which() function:

# Often we have a list and want to know the index or indices of the elements in the list where some condition is true. This is exactly what the which() function is designed to do!

# Here are some examples
x = c(1,2,3,4,5,12,11,10,9,8,7,6)
cat('List of values x\n')
print(x)
cat('\n')

#----------------
# Finding the index and value of the maximum
i = which(x == max(x)) # index or indices of max
v = x[i] # value of the max
cat('Index where x has max:\n')
print(i)
cat('Corresponding value\n')
print(x[i])
cat('\n')

#----------------
# Finding all indices where x < 8
i = which(x < 8)  # list of indices
v = x[i] # list of values
cat('Indices where x < 8:\n')
print(i)
cat('Corresponding values\n')
print(x[i])
cat('\n')

#----------------
# When we have a matrix structure, which can find us the array indices we want
y = matrix(c(1,2,3,4,5,12,11,10,9,8,7,6), nrow=3, ncol=4)
cat('Matrix y\n')
print(y)
cat('\n')

#----------------
# With arr.ind=TRUE, which() returns the array index where the logical statement is true
i = which(y == max(y), arr.ind=TRUE)
cat('Array index where matrix y has max:\n')
print(i)
cat('Corresponding value\n')
print(y[i])
cat('\n')

#----------------
# Without arr.ind=TRUE, which() ignores the matrix structure on y
cat('Index where y has max --IGNORING matrix structure:\n')
i = which(y == max(y)) 
print(i)
cat('Corresponding max\n')
print(y[i])
cat('\n')

#----------------
# Note: If the logical statement is true at several indices then which returns all of them. For example:
i = which(y < 9, arr.ind=TRUE)
cat('Array indices where y < 9\n')
print(i)
cat('Corresponding values\n')
print(y[i])
cat('\n')

#----------------
# A typical use of finding the max.
cat('Typical use of which() to find a matrix max.\n')
x = c(0.1, 0.2, 0.3, 0.4, 0.5)
y = c(0.1, 0.2, 0.3, 0.4, 0.5)
cat('x and y arrays:\n')
print(x)
print(y)

# mat holds values of the funcion f(x,y)=x/y+y/x
mat = matrix(0, nrow=length(x), ncol=length(y))
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    mat[i,j] = x[i]/y[j] + x[j]/y[i]
  }
}
cat('Matrix holding values of f(x,y):\n')
print(mat)
# Problem: Find the values of a and b corresponding to the maximum value in mat.

# First: find the array indices where the max occurs
max_indices = which(mat==max(mat), arr.ind=TRUE)
# max_indices is an n x 2 array, where n is the number of matrix entries that equal max(mat). In this case, n == 6.
cat('The array mat has maxima at the following indices:\n')
print(max_indices)

# Now pull out the list of x and y values that give that max
x_indices = max_indices[,1]
y_indices = max_indices[,2]
xvals_max = x[x_indices]
yvals_max = y[y_indices]

# The nicest way to store and display these values is to use cbind
xy_max = cbind(xvals_max, yvals_max)
cat('List of x,y values that give the maximum:\n')
print(xy_max)
cat('The maximum value:\n')
print(max(mat))
#----------------
