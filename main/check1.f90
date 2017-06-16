
! THE BEER-WARE LICENSE:
! Alberto Ramos wrote this file. As long as you retain this 
! notice you can do whatever you want with this stuff. If we meet some 
! day, and you think this stuff is worth it, you can buy me a beer in 
! return. <alberto.ramos@cern.ch>
!
! $ v1.0 $
!                               

program check

  use uncertainties

  type (ureal) :: x, y, z, w1, w2

  x = (/3.0_DP,0.1_DP/)
  y = (/12.0_DP,0.1_DP/)
  z = (/2.0_DP,0.2_DP/)
  write(*,*)'x:   ', x%x, sqrt(sum(x%del**2))
  write(*,*)'y:   ', y%x, sqrt(sum(y%del**2))
  write(*,*)'z:   ', z%x, sqrt(sum(z%del**2))

  w1 = ((x+y)*((x-y)/z) + y/z) * z - x**2+y*y
  w2 = w1 - y
  write(*,*)'zero: ', w2%x, sqrt(sum(w2%del**2))

  
  stop
end program check
