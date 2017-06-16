!
! A MODULE 
!                               
! "THE BEER-WARE LICENSE":
! Alberto Ramos wrote this file. As long as you retain this 
! notice you can do whatever you want with this stuff. If we meet some 
! day, and you think this stuff is worth it, you can buy me a beer in 
! return. <alberto.ramos@cern.ch>
!
! $ v1.0 201XXXXX $
!                               

submodule (uncertainties) uncertainties_func

  implicit none
  
  real (kind=DP), parameter :: LGe10_DP = log(10.0_DP)
  integer, allocatable :: ws_map(:,:), ws_ids(:)
  integer :: ns = -1

  
contains

! ********************************
! *
  module elemental function usqrt(a)
! *
! ********************************
    type (ureal), intent (in) :: a
    type (ureal) :: usqrt

    call usqrt%init(a%nid)
    usqrt%id = a%id

    usqrt%x   = sqrt(a%x)
    usqrt%del = a%del/(2.0_DP*usqrt%x)
       
    return
  end function usqrt

! ********************************
! *
  module elemental function ulog(a)
! *
! ********************************
    type (ureal), intent (in) :: a
    type (ureal) :: ulog

    call ulog%init(a%nid)
    ulog%id = a%id

    ulog%x   = log(a%x)
    ulog%del = a%del/a%x
       
    return
  end function ulog

! ********************************
! *
  module elemental function ulog10(a)
! *
! ********************************
    type (ureal), intent (in) :: a
    type (ureal) :: ulog10

    call ulog10%init(a%nid)
    ulog10%id = a%id

    ulog10%x   = log(a%x)
    ulog10%del = a%del/a%x/LGe10_DP
       
    return
  end function ulog10

! ********************************
! *
  module elemental function uexp(a)
! *
! ********************************
    type (ureal), intent (in) :: a
    type (ureal) :: uexp

    call uexp%init(a%nid)
    uexp%id = a%id

    uexp%x   = exp(a%x)
    uexp%del = exp(a%x)*a%del
       
    return
  end function uexp

! ********************************
! *
  module elemental function usin(a)
! *
! ********************************
    type (ureal), intent (in) :: a
    type (ureal) :: usin

    call usin%init(a%nid)
    usin%id = a%id

    usin%x   = sin(a%x)
    usin%del = cos(a%x)*a%del
       
    return
  end function usin

! ********************************
! *
  module elemental function ucos(a)
! *
! ********************************
    type (ureal), intent (in) :: a
    type (ureal) :: ucos

    call ucos%init(a%nid)
    ucos%id = a%id

    ucos%x   =  cos(a%x)
    ucos%del = -sin(a%x)*a%del
           
    return
  end function ucos
  
end submodule uncertainties_func
