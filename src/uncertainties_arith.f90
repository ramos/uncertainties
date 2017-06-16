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

submodule (uncertainties) uncertainties_arith

  implicit none
  
  integer, allocatable :: ws_map(:,:), ws_ids(:)
  integer :: ns = -1
  
contains

! ********************************
! *
  module subroutine covariance(mat,ud)
! *
! ********************************
    type (ureal), intent (in) :: ud(:)
    real (kind=DP), intent (out) :: mat(size(ud),size(ud))
    integer :: nd, n1, n2, i, nid, idx1, idx2, ism
    
    ism = sum(ud(:)%nid)
    call init_ws(ism)

    nd = size(ud)
    mat = 0.0_DP
    do n1 = 1, nd
       do n2 = n1, nd
          call combine_ids(ws_map,ws_ids,nid,ud(n1),ud(n2))
          do i = 1, nid
             idx1 = ws_map(i,1)
             idx2 = ws_map(i,2)
             if ( (idx1.ne.0).and.(idx2.ne.0) ) then
                mat(n1,n2) = mat(n1,n2) + ud(n1)%del(idx1)*ud(n2)%del(idx2)
             end if
          end do
       end do
    end do
    
    do n1 = 1, nd
       do n2 = n1, nd
          mat(n2,n1) = mat(n1,n2)
       end do
    end do

    return
  end subroutine covariance
  
! ********************************
! *
  subroutine init_ws(n)
! *
! ********************************
    integer, intent (in) :: n
    
    if (ns.ge.n) return
    if (allocated(ws_map)) deallocate(ws_map)
    if (allocated(ws_ids)) deallocate(ws_ids)

    ns = n
    allocate(ws_map(ns,2), ws_ids(ns))
    
    return
  end subroutine init_ws
  
! ********************************
! *
  module function add_cr(a,r)
! *
! ********************************
    type (ureal), intent (in)   :: a
    real (kind=DP), intent (in) :: r
    type (ureal) :: add_cr
    integer :: nid

    nid = a%nid
    call init_ws(a%nid)
    call add_cr%init(a%nid)
    add_cr%id(1:nid) = a%id(1:nid)
    
    add_cr%x = a%x + r
    add_cr%del = a%del

    return
  end function add_cr
  
! ********************************
! *
  module function add_cl(r,a)
! *
! ********************************
    type (ureal), intent (in)   :: a
    real (kind=DP), intent (in) :: r
    type (ureal) :: add_cl
    integer :: nid
    
    nid = a%nid
    call init_ws(a%nid)
    call add_cl%init(a%nid)
    add_cl%id(1:nid) = a%id(1:nid)
    
    add_cl%x = a%x + r
    add_cl%del = a%del

    return
  end function add_cl
  
! ********************************
! *
  module function add_t(a,b)
! *
! ********************************
    type (ureal), intent (in) :: a, b
    type (ureal) :: add_t
    real (kind=DP) :: sa, sb
    integer :: i, nid
    
    call init_ws(a%nid+b%nid)
    call combine_ids(ws_map,ws_ids,nid,a,b)
    call add_t%init(nid)
    add_t%id(1:nid) = ws_ids(1:nid)
    
    add_t%x = a%x + b%x
    do i = 1, add_t%nid
       if (ws_map(i,1).ne.0) then
          sa = a%del(ws_map(i,1))
       else
          sa = 0.0_DP
       end if
       if (ws_map(i,2).ne.0) then
          sb = b%del(ws_map(i,2))
       else
          sb = 0.0_DP
       end if
       
       add_t%del(i) = sa + sb
    end do
    
    return
  end function add_t
  
! ********************************
! *
  module function sub_cr(a,r)
! *
! ********************************
    type (ureal), intent (in)   :: a
    real (kind=DP), intent (in) :: r
    type (ureal) :: sub_cr
    integer :: nid
    
    nid = a%nid
    call init_ws(a%nid)
    call sub_cr%init(a%nid)
    sub_cr%id(1:nid) = a%id(1:nid)
    
    sub_cr%x = a%x - r
    sub_cr%del = a%del

    return
  end function sub_cr
  
! ********************************
! *
  module function sub_cl(r,a)
! *
! ********************************
    type (ureal), intent (in)   :: a
    real (kind=DP), intent (in) :: r
    type (ureal) :: sub_cl
    integer :: nid
    
    nid = a%nid
    call init_ws(a%nid)
    call sub_cl%init(a%nid)
    sub_cl%id(1:nid) = a%id(1:nid)
    
    sub_cl%x   = r - a%x
    sub_cl%del = -a%del
    
    return
  end function sub_cl
  
! ********************************
! *
  module function sub_t(a,b)
! *
! ********************************
    type (ureal), intent (in) :: a, b
    type (ureal) :: sub_t
    real (kind=DP) :: sa, sb
    integer :: i, nid
    
    call init_ws(a%nid+b%nid)
    call combine_ids(ws_map,ws_ids,nid,a,b)
    call sub_t%init(nid)
    sub_t%id(1:nid) = ws_ids(1:nid)
    
    sub_t%x = a%x - b%x
    do i = 1, sub_t%nid
       if (ws_map(i,1).ne.0) then
          sa = a%del(ws_map(i,1))
       else
          sa = 0.0_DP
       end if
       if (ws_map(i,2).ne.0) then
          sb = b%del(ws_map(i,2))
       else
          sb = 0.0_DP
       end if
       sub_t%del(i) = sa - sb
    end do
    
    return
  end function sub_t
  
! ********************************
! *
  module function mul_t(a,b)
! *
! ********************************
    type (ureal), intent (in) :: a, b
    type (ureal) :: mul_t
    real (kind=DP) :: sa, sb
    integer :: i, nid
    
    call init_ws(a%nid+b%nid)
    call combine_ids(ws_map,ws_ids,nid,a,b)
    call mul_t%init(nid)
    mul_t%id(1:nid) = ws_ids(1:nid)
    
    mul_t%x = a%x * b%x
    do i = 1, mul_t%nid
       if (ws_map(i,1).ne.0) then
          sa = a%del(ws_map(i,1))
       else
          sa = 0.0_DP
       end if
       if (ws_map(i,2).ne.0) then
          sb = b%del(ws_map(i,2))
       else
          sb = 0.0_DP
       end if
       mul_t%del(i) = sa*b%x + sb*a%x
    end do
    
    return
  end function mul_t
  
! ********************************
! *
  module function mul_cl(r,a)
! *
! ********************************
    type (ureal), intent (in)   :: a
    real (kind=DP), intent (in) :: r
    type (ureal) :: mul_cl
    integer :: nid
    
    nid = a%nid
    call init_ws(a%nid)
    call mul_cl%init(a%nid)
    mul_cl%id(1:nid) = a%id(1:nid)
    
    mul_cl%x   = r * a%x
    mul_cl%del = r*a%del
    
    return
  end function mul_cl
  
! ********************************
! *
  module function mul_cr(a,r)
! *
! ********************************
    type (ureal), intent (in)   :: a
    real (kind=DP), intent (in) :: r
    type (ureal) :: mul_cr
    integer :: nid
    
    nid = a%nid
    call init_ws(a%nid)
    call mul_cr%init(a%nid)
    mul_cr%id(1:nid) = a%id(1:nid)
    
    mul_cr%x   = r * a%x
    mul_cr%del = r*a%del
    
    return
  end function mul_cr
  
! ********************************
! *
  module function div_t(a,b)
! *
! ********************************
    type (ureal), intent (in) :: a, b
    type (ureal) :: div_t
    real (kind=DP) :: sa, sb
    integer :: i, nid
    
    call init_ws(a%nid+b%nid)
    call combine_ids(ws_map,ws_ids,nid,a,b)
    call div_t%init(nid)
    div_t%id(1:nid) = ws_ids(1:nid)
    
    div_t%x = a%x / b%x
    do i = 1, div_t%nid
       if (ws_map(i,1).ne.0) then
          sa = a%del(ws_map(i,1))
       else
          sa = 0.0_DP
       end if
       if (ws_map(i,2).ne.0) then
          sb = b%del(ws_map(i,2))
       else
          sb = 0.0_DP
       end if
       div_t%del(i) = sa/b%x - sb*a%x/b%x**2
    end do
    
    return
  end function div_t
  
! ********************************
! *
  module function div_cl(r,a)
! *
! ********************************
    type (ureal), intent (in)   :: a
    real (kind=DP), intent (in) :: r
    type (ureal) :: div_cl
    integer :: nid
    
    nid = a%nid
    call init_ws(a%nid)
    call div_cl%init(a%nid)
    div_cl%id(1:nid) = a%id(1:nid)
    
    div_cl%x   = r / a%x
    div_cl%del = -r/a%x**2 * a%del
    
    return
  end function div_cl
  
! ********************************
! *
  module function div_cr(a,r)
! *
! ********************************
    type (ureal), intent (in)   :: a
    real (kind=DP), intent (in) :: r
    type (ureal) :: div_cr
    integer :: nid
    
    nid = a%nid
    call init_ws(a%nid)
    call div_cr%init(a%nid)
    div_cr%id(1:nid) = a%id(1:nid)
    
    div_cr%x   = a%x / r
    div_cr%del = a%del / r
    
    return
  end function div_cr
  
! ********************************
! *
  module function pow_t(a,n)
! *
! ********************************
    type (ureal), intent (in) :: a
    integer, intent (in)      :: n
    type (ureal) :: pow_t
    integer :: i, nid

    nid = a%nid
    call pow_t%init(nid)
    pow_t%id(1:nid) = a%id(1:nid)
    
    pow_t%x = a%x ** n
    do i = 1, pow_t%nid
       pow_t%del(i) = real(n,kind=DP)*a%x**(n-1) * a%del(i)
    end do
    
    return
  end function pow_t
  
! ********************************
! *
  subroutine combine_ids(map,ids,nid,a,b)
! *
! ********************************
    type (ureal), intent (in) :: a, b
    integer, intent (out) :: map(:,:), ids(:), nid
    integer :: i, j
    logical :: fnd
    

    map = 0
    nid = a%nid
    forall (i=1:nid) map(i,1) = i
    ids(1:nid) = a%id(:)
    do i = 1, b%nid
       
       fnd = .false.
       do j = 1, a%nid
          if (b%id(i).eq.a%id(j)) then
             map(j,2) = i
             fnd = .true.
             exit
          end if
       end do
       
       if (.not.fnd) then
          nid = nid + 1
          ids(nid)   = b%id(i)
          map(nid,2) = i
       end if
       
    end do
    
    
    return
  end subroutine combine_ids

end submodule uncertainties_arith
