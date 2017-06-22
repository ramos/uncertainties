
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

  type (ureal) :: x, y, z, w1, w2, w3, r1, r2

  x = (/3.0_DP,0.1_DP/)
  y = (/1.0_DP,0.1_DP/)
  z = (/1.0_DP,0.1_DP/)


  write(*,'(1A)', advance="NO")'x:   '
  call x%write()                       ! x:   3.0000000000 +/- .100000
  write(*,'(1A)', advance="NO")'y:   '
  call y%write()                       ! y:   1.0000000000 +/- .100000
  write(*,*)
  
  w1 = usin(x+y)
  w2 = usin(x+z)
  w3 = (usin(x)*ucos(y) + ucos(x)*usin(y))

  write(*,'(1A)', advance="NO")'w1: '
  call w1%write()                       
  write(*,'(1A)', advance="NO")'w2: '
  call w2%write()
  write(*,'(1A)', advance="NO")'w3: '
  call w3%write()
  write(*,*)

  r1 = w1 - w3
  r2 = w2 - w3

  write(*,'(1A)', advance="NO")'result (w1-w3): '
  call r1%write()                      
  write(*,'(1A)', advance="NO")'result (w2-w3): '
  call r2%write()

  stop
end program check
