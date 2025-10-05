#---------------------------------------------------------
# File:   MIT18_05S22_display_all_colors.r
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
# Simple tool to display an array of all the colors

# colors() = builtin function with (at this point) 657 colors
# Note there are a lot of repeats e.g. gray33 and grey33

plot(c(0,0,22,22),c(0,35,0,35),type='n')  #empty plot to establish size

my_display_col = function(cc) {
    #cc = array of colors
   n = length(cc)
   m1 = floor(1.2*sqrt(n))
   m2 = floor(n/m1 +1)
   plot(c(0,0,m2+3,m2+3), c(0,m1+3,0,m1+3),type='n')  #empty plot to establish size
   for (i in 1:m1) { #rows
     for (j in 1:m2) {#columns
         ind = (i-1)*m2 + j
         if (ind <= n){
             rect(j,i,j+1,i+1,col=cc[ind])
         }
     }
   }
}

print("Showing colors()")
my_display_col(colors())
readline("Hit return for heat.colors(200)")
my_display_col(heat.colors(200))
readline("Hit return for terrain.colors(500)")
my_display_col(terrain.colors(500))
readline("Hit return for rainbow(500)")
my_display_col(rainbow(500))
readline("Hit return for topo.colors(500)")
my_display_col(topo.colors(500))
readline("Hit return for cm.colors(500)")
my_display_col(cm.colors(500))
