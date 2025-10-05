#---------------------------------------------------------
# File:   MIT18_05S22_mathtext.r
# Author: Jeremy Orloff
# Adding math symbols to plots

# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

# Tutorial:
# Look at ?mathplot for more help

show_ex1 = TRUE
show_ex2 = TRUE

#---------------------------------------------------------
if (show_ex1) {
  # Using bquote
  # Rules:
  # Strings – Require quotes wrapped w/ tilde separator (e.g., "my text"  ~).
  # Math Expressions – Unquoted & follow ?plotmath
  #   r[x] --x is a subscript
  #   r^x -- x is a superscript
  # Numbers – Unquoted when part of math notation
  # Variables – Use .() (pass in string or numeric), i.e. .(x) means use the value of x. Note, .(x) places a leading 0 or -0 where needed in front of the decimal point
  # ~ separates items and prints to a space
  # * separates items and doesn't include a space
  
  cor <- -.321
  cor2 <- '-.321'
  
  plot(1:10, 1:10, main = bquote("Hello" ~ r[xy] == .(cor) ~ "and" ~ B^2))
  text(7,2, bquote("Hel" * "lo" ~ r[xy] == .(cor2) ~ "and" * B^2*","~hat(theta)))
}

if (show_ex2) {
  #make a plot
  plot(c(0,5), c(0,5))

  #straight text
  y=5
  text(0, y, adj=0, "see ?plotmath for more help") 
  text(2, y, adj=0, "straight text")

  #symbols
  y = 4
  text(0, y, adj=0, "Symbols:")
  # expression returns an unevaluated S expression
  text(1, y, adj=0, expression(over(a[n]*beta^2, 5)), cex=1.5)
  text(2, y, adj=0, cex=1.5, expression(paste(plain(sin) * phi, "  and  ", plain(cos) * phi)))

  #mix symbols and text
  ## note that both of these use calls rather than expressions.
  y = 3
  text(0, y, adj=0, "Symbols and values:")
  theta = 1.23
  text(2, y, adj=0, bquote(hat(theta) == .(theta))) #inside bquote, .(x) means print the value of x
  #substitute substitutes values (this subs for a, b not y,alpha
  text(3, y,adj=0, substitute(y==a +alpha*v[1] + b*v[2], list(a=5,b=6)))
  i = 3
  text(4, y, adj=0, substitute(list(xi,eta) == group("(",list(x,y),")"), list(x=i, y=i+1)))
  
  y=2
  param.list=list(mu1 = 0, mu2 = 0, s1 = 3, s2 = 2, s3 = 2, s4 = 4)
  ## typeset density of 2-var. normal dist.
  ex = expression(f(x) == frac(1, sqrt((2 * pi)^n ~~ det(Sigma[x]))) ~~ exp * bgroup("(",-frac(1, 2) ~~ (x - mu)^T * Sigma[x]^-1 *(x - mu), ")"))
  text(0, y, adj = 0, labels = ex)
  #To use two == need to use lists
  ex = substitute(list("with " * mu == bgroup("(",atop(mu1,mu2), ")"),
                       Sigma[x] == bgroup("(", atop(s1 ~~ s3, s2 ~~ s4), ")")),
                  param.list)
  text(2.2, y, adj=0, labels = ex)
  
  
  #legends
  y = 1
  text(0, y, adj=0, "Legend requires use of do.call")
  a = 3; b = 5
  legend1 <- substitute(alpha == a, list(a = a))
  legend2 <- substitute(beta == b, list(b = b))
  lty = c("dashed","dotted")
  col = c("red","blue")
  legend(2, y, adj=0, do.call("expression", list(legend1, legend2)), lty=lty,col=col)
}
