!
! A MODULE for linear propagation of errors
!                               
! "THE BEER-WARE LICENSE":
! Alberto Ramos wrote this file. As long as you retain this 
! notice you can do whatever you want with this stuff. If we meet some 
! day, and you think this stuff is worth it, you can buy me a beer in 
! return. <alberto.ramos@cern.ch>
!
! $ v1.0 20170527 $
!                               

module uncertainties

  USE ISO_FORTRAN_ENV, Only : error_unit, output_unit

  IMPLICIT NONE

  private

  integer, parameter :: DP = Kind(1.D0)

  public :: DP
  
  type :: ureal
     real (kind=DP) :: x
     real (kind=DP), allocatable :: del(:)
     integer, allocatable :: id(:)
     integer :: nid=-1
   contains
     procedure :: init  => init_t
     procedure :: write => write_t
  end type ureal

  public :: ureal, uerror, uvalue
  
  interface assignment (=) 
     module procedure equal_v, equal_t
  end interface assignment (=)
  
  interface operator (+)
     module procedure add_t, add_cr, add_cl
  end interface operator (+)
  interface operator (-)
     module procedure sub_t, sub_cr, sub_cl
  end interface operator (-)
  interface operator (*)
     module procedure mul_t, mul_cr, mul_cl
  end interface operator (*)
  interface operator (/)
     module procedure div_t, div_cr, div_cl
  end interface operator (/)
  interface operator (**)
     module procedure pow_t
  end interface operator (**)

  interface
     module subroutine covariance(mat,ud)
       type (ureal), intent (in) :: ud(:)
       real (kind=DP), intent (out) :: mat(size(ud),size(ud))
     end subroutine covariance
     module function add_t(a,b)
       type (ureal), intent (in) :: a, b
       type (ureal) :: add_t
     end function add_t
     module function add_cr(a,r)
       type (ureal), intent (in)   :: a
       real (kind=DP), intent (in) :: r
       type (ureal) :: add_cr
     end function add_cr
     module function add_cl(r,a)
       type (ureal), intent (in)   :: a
       real (kind=DP), intent (in) :: r
       type (ureal) :: add_cl
     end function add_cl
     module function sub_t(a,b)
       type (ureal), intent (in) :: a, b
       type (ureal) :: sub_t
     end function sub_t
     module function sub_cr(a,r)
       type (ureal), intent (in)   :: a
       real (kind=DP), intent (in) :: r
       type (ureal) :: sub_cr
     end function sub_cr
     module function sub_cl(r,a)
       type (ureal), intent (in)   :: a
       real (kind=DP), intent (in) :: r
       type (ureal) :: sub_cl
     end function sub_cl
     module function mul_t(a,b)
       type (ureal), intent (in) :: a, b
       type (ureal) :: mul_t
     end function mul_t
     module function mul_cr(a,r)
       type (ureal), intent (in)   :: a
       real (kind=DP), intent (in) :: r
       type (ureal) :: mul_cr
     end function mul_cr
     module function mul_cl(r,a)
       type (ureal), intent (in)   :: a
       real (kind=DP), intent (in) :: r
       type (ureal) :: mul_cl
     end function mul_cl
     module function div_t(a,b)
       type (ureal), intent (in) :: a, b
       type (ureal) :: div_t
     end function div_t
     module function div_cr(a,r)
       type (ureal), intent (in)   :: a
       real (kind=DP), intent (in) :: r
       type (ureal) :: div_cr
     end function div_cr
     module function div_cl(r,a)
       type (ureal), intent (in)   :: a
       real (kind=DP), intent (in) :: r
       type (ureal) :: div_cl
     end function div_cl
     module function pow_t(a,n)
       type (ureal), intent (in) :: a
       integer, intent (in)      :: n
       type (ureal) :: pow_t
     end function pow_t
  end interface
  
  integer, save :: id_count = 0

  public :: covariance
  public :: assignment(=), operator(+), operator(-), &
       operator(*), operator (/), operator (**)

  interface
     module elemental function usin(a)
       type (ureal), intent (in) :: a
       type (ureal) :: usin
     end function usin
     module elemental function ucos(a)
       type (ureal), intent (in) :: a
       type (ureal) :: ucos
     end function ucos
     module elemental function usqrt(a)
       type (ureal), intent (in) :: a
       type (ureal) :: usqrt
     end function usqrt
     module elemental function ulog(a)
       type (ureal), intent (in) :: a
       type (ureal) :: ulog
     end function ulog
     module elemental function ulog10(a)
       type (ureal), intent (in) :: a
       type (ureal) :: ulog10
     end function ulog10
     module elemental function uexp(a)
       type (ureal), intent (in) :: a
       type (ureal) :: uexp
     end function uexp
  end interface

  public :: usin, ucos, usqrt, ulog, uexp
  
contains

! ********************************
! *
  function uvalue(a)
! *
! ********************************
    type (ureal), intent (in) :: a
    real (kind=DP) :: uvalue

    uvalue = a%x
    
    return
  end function uvalue

! ********************************
! *
  function uerror(a)
! *
! ********************************
    type (ureal), intent (in) :: a
    real (kind=DP) :: uerror

    uerror = sqrt(sum(a%del**2))
    
    return
  end function uerror

! ********************************
! *
  subroutine write_t(a,ifn)
! *
! ********************************
    class (ureal), intent (in) :: a
    integer, intent (in), optional :: ifn

    integer :: ifl, nv, ne
    character (len=100) :: cv, ce
    character (len=10)  :: cerr
    real (kind=DP) :: err
    
    ifl = output_unit
    if (present(ifn)) ifl = ifn

    err = sqrt(sum(a%del**2))
!!$    nv  = int(log10(a%x))
!!$    ne  = int(log10(err))
!!$    
!!$    write(*,*)a%x, err
!!$    write(cv,'(1F0.8)')a%x
!!$    write(ce,'(1ES31.25)')err
!!$    write(cerr,'(2A1,1A2,1A1)')'(',ce(1:1), ce(3:4),')'
!!$    write(*,*)'aqui: ', trim(cerr)
    
    write(ifl,'(1F0.10,1X,1A,1X,1F0.6)')a%x, '+/-', err

    return
  end subroutine write_t
    


  
! ********************************
! *
  elemental subroutine free_t(a)
! *
! ********************************
    type (ureal), intent (inout) :: a

    a%nid = -1
    if (allocated(a%del)) deallocate(a%del)
    if (allocated(a%id)) deallocate(a%id)

    return
  end subroutine free_t
    
! ********************************
! *
  elemental subroutine init_t(a,n)
! *
! ********************************
    class (ureal), intent (inout) :: a
    integer, intent (in) :: n
    
    if (a%nid.ne.n) then
       call free_t(a)
       allocate(a%del(n),a%id(n))
       a%nid = n
    end if
    a%x   = 0.0_DP
    a%del = 0.0_DP
        
    return
  end subroutine init_t
    
! ********************************
! *
    subroutine equal_v(a,v)
! *
! ********************************
      real (kind=DP), intent (in)  :: v(2)
      type (ureal), intent (out) :: a

      call init_t(a,1)
      a%x      = v(1)
      a%del(1) = v(2)
      a%id(1)  = id_count
      
      id_count = id_count + 1
      
      return
    end subroutine equal_v
  
! ********************************
! *
    subroutine equal_t(a,b)
! *
! ********************************
      type (ureal), intent (inout) :: a
      type (ureal), intent (in)  :: b

      call init_t(a,b%nid)
      a%x   = b%x
      a%del = b%del
      a%id  = b%id 
      
      return
    end subroutine equal_t
  
! ********************************
! *
    Subroutine module_error(routine, msg)
! *
! ********************************

      Character (len=*), Intent (in) :: routine, msg

      Write(error_unit,*)'In '//Trim(routine)// ' :'
      Write(error_unit,'(5X,1A)')Trim(msg)
      Write(error_unit,*)

      stop
    end subroutine module_error

  end Module uncertainties

