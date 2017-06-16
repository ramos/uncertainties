
A MODULE for linear propagation of errors

This module implements linear propagation of errors, including
correlations of variables.

** USAGE **

* Variables with uncertainties are defined as type (ureal), and
initialized by assigning them to a 2-component 1d array.

  use uncertainties

  type (ureal) :: a

  a = (/1.0D0,0.1D0/) ! variable a has value 1.0 and error 0.1

* One can perform normal operations with variables

  type (ureal) :: a, b, c

  a = (/1.0D0,0.1D0/) ! variable a has value 1.0 and error 0.1
  b = (/2.0D0,0.3D0/) ! variable b has value 2.0 and error 0.3

  c = a+b ! variable c has value 3.0 and error sqrt(0.3**2+0.1**2)

* Intric functions work by appending u to the function name.

  type (ureal) :: a, b, c

  a = (/1.0D0,0.1D0/) ! variable a has value 1.0 and error 0.1
  b = (/2.0D0,0.3D0/) ! variable b has value 2.0 and error 0.3

  c = usin(a) + usqrt(b) 

* All initialized variables are assumed uncorrelated. After,
  all correlations are taken into account automatically

  type (ureal) :: a, b, c, d

  a = (/1.0D0,0.1D0/) ! variable a has value 1.0 and error 0.1
  b = (/2.0D0,0.3D0/) ! variable b has value 2.0 and error 0.3

  c = (a+b)**2 

  d = c - (a**2 + b**2 +2*a*b) ! Variable d has value 0 and error 0

"THE BEER-WARE LICENSE":
Alberto Ramos wrote this file. As long as you retain this 
notice you can do whatever you want with this stuff. If we meet some 
day, and you think this stuff is worth it, you can buy me a beer in 
return. <alberto.ramos@cern.ch>

 
