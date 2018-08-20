! p is the point in rzphi
! dphi is the distance in phi to follow

subroutine follow_field(p, dphi, dist, istate)


  use coil_module


  parameter (lrw=58)
  parameter (liw=22)
  parameter (itask=1)

  double precision, dimension(3) :: p, pold, pnew, poldc, pnewc
  double precision, dimension(2) :: y
  ! For now this is unused.
  double precision :: dphi, t0, t1, tol
  ! dstuff for dlsode
  integer :: ifail, istate
  double precision, dimension(liw) :: iwork
  double precision, dimension(lrw) :: rwork
  double precision, dimension(1) :: dist, r1, z1, phi1, r2, z2, phi2, x1, x2, y1, y2

  external field_deriv
  
  ! start by resetting dist to zero
  dist=0.0

  y(1) = p(1)
  y(2) = p(2)

  ! start and stop phi values
  t0 = p(3)
  t1 = p(3) + dphi
  
  ! keep track of old point
  r1=y(1)
  z1=y(2)
  phi1=t0
  pold=(/r1, z1, phi1/)

  tol = 1.e-8

  ifail = 0
  istate = 1

  call dlsode(field_deriv, 2, y, t0, t1, 1, tol, tol, itask, istate, 0, &
       rwork, lrw, iwork, liw, jacl, 22)

  ! This indicates that dlsode exited improperly
  if (istate < 0) then
     dist = 0.
     return
  end if

  p(1) = y(1)
  p(2) = y(2)
  p(3) = t1
  

  ! keep track of new point
  r2=p(1)
  z2=p(2)
  phi2=p(3)
  pnew=(/r2,z2,phi2/)
  
  ! calculate distance between original point and new point
  ! accumulate distance and call connection length
  
  ! first, convert to cartesian so we can use the distance formula
  call pol2cart(pold,poldc)
  call pol2cart(pnew,pnewc)
  
  x1=poldc(1)
  y1=poldc(2)
  
  x2=pnewc(1)
  y2=pnewc(2)
  
  dist=sqrt((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)
  
  ! print *, dist
  
  
  
  return
end subroutine follow_field

!like follow field but integrating along field line
!instead of along r,z,phi
subroutine follow_field_s(p, ds, istate)

  use coil_module


  parameter (lrw=58)
  parameter (liw=23)
  parameter (itask=1)

  double precision, dimension(3) :: y,p
  ! For now this is unused.
  double precision :: ds, t0, t1, tol
  ! dstuff for dlsode
  integer :: ifail, istate
  double precision, dimension(liw) :: iwork
  double precision, dimension(lrw) :: rwork

  external field_deriv_s

  y(1) = p(1)
  y(2) = p(2)
  y(3) = p(3)

  ! start and stop phi values
  t0 = 0
  t1 = ds
  
  tol = 1.e-8

  ifail = 0
  istate = 1

  call dlsode(field_deriv_s, 3, y, t0, t1, 1, tol, tol, itask, istate, 0, &
       rwork, lrw, iwork, liw, jacl, 22)

  ! This indicates that dlsode exited improperly
  if (istate < 0) then
     dist = 0.
     return
  end if

  p(1) = y(1)
  p(2) = y(2)
  p(3) = y(3)  
  
  return
end subroutine follow_field_s

! a version of arc length field following that includes the
! psi calculation, it requires an initialized value of psi, not sure
! how to get that, guess it's getable from vmec?
subroutine follow_field_s_wpsi(p, psi, ds, istatic)


  use coil_module


  parameter (lrw=112)
  parameter (liw=26)
  parameter (itask=1)

  double precision, dimension(3) :: p,psi
  double precision, dimension(6) :: y
  ! For now this is unused.
  double precision :: ds, t0, t1, tol
  ! dstuff for dlsode
  integer :: ifail, istate
  double precision, dimension(liw) :: iwork
  double precision, dimension(lrw) :: rwork

  external field_deriv_s_wpsi

  y(1) = p(1)
  y(2) = p(2)
  y(3) = p(3)
  y(4) = psi(1)
  y(5) = psi(2)
  y(6) = psi(3)

  ! start and stop phi values
  t0 = 0
  t1 = ds
  
  tol = 1.e-8

  ifail = 0
  istate = 1

  call dlsode(field_deriv_s_wpsi, 6, y, t0, t1, 1, tol, tol, itask, istate, 0, &
       rwork, lrw, iwork, liw, jacl, 22)

  ! This indicates that dlsode exited improperly
  if (istate < 0) then
     dist = 0.
     return
  end if

  p(1) = y(1)
  p(2) = y(2)
  p(3) = y(3)
  psi(1) = y(4)
  psi(2) = y(5)
  psi(3) = y(6)
  
  return
end subroutine follow_field_s_wpsi

!Integration using chi as the integrating parameter
!input point is in r,z,phi
subroutine follow_field_chi(p, ds, dist, istate, iter)

  use coil_module

  parameter (lrw=58)
  parameter (liw=23)
  parameter (itask=1)

  real, dimension(3) :: y,p,pxyz0,pxyz1
  ! For now this is unused.
  real :: ds, t0, t1, tol
  ! dstuff for dlsode
  integer :: ifail, istate
  real, dimension(liw) :: iwork
  real, dimension(lrw) :: rwork

  external field_deriv_chi

  y(1) = p(1)
  y(2) = p(2)
  y(3) = p(3)

  call pol2cart(p,pxyz0)

  ! start and stop phi values
  t0 = (iter-1)*ds 
  t1 = t0 + ds
  tol = 1.e-8

  ifail = 0
  istate = 1

  call dlsode(field_deriv_chi, 3, y, t0, t1, 1, tol, tol, itask, istate, 0, &
       rwork, lrw, iwork, liw, jacl, 22)

  ! This indicates that dlsode exited improperly
  if (istate < 0) then
     dist = 0.
     return
  end if

  p(1) = y(1)
  p(2) = y(2)
  p(3) = y(3)

  call pol2cart(p,pxyz1)
  dist=sqrt((pxyz1(1)-pxyz0(1))**2+ &
       (pxyz1(2)-pxyz0(2))**2+ &
       (pxyz1(3)-pxyz0(3))**2)
  
  return
end subroutine follow_field_chi

subroutine follow_field_gboozer(p, dphi, dist, istate)


  use coil_module


  parameter (lrw=58)
  parameter (liw=22)
  parameter (itask=1)

  real, dimension(3) :: pold, pnew, poldc, pnewc
  real, dimension(4) :: p
  real, dimension(2) :: y
  ! For now this is unused.
  real :: dphi, t0, t1, tol
  ! dstuff for dlsode
  integer :: ifail, istate
  real, dimension(liw) :: iwork
  real, dimension(lrw) :: rwork
  real, dimension(1) :: dist, r1, z1, phi1, r2, z2, phi2, x1, x2, y1, y2

  external field_deriv_gboozer

  ! start by resetting dist to zero
  dist=0.0

  y(1) = p(1)
  y(2) = p(2)

  ! start and stop phi values
  t0 = p(4)
  t1 = p(4) + dphi

  ! keep track of old point
  r1=y(1)
  z1=y(2)
  phi1=t0
  pold=(/r1, z1, phi1/)

  tol = 1.e-8

  ifail = 0
  istate = 1

  call dlsode(field_deriv_, 2, y, t0, t1, 1, tol, tol, itask, istate, 0, &
       rwork, lrw, iwork, liw, jacl, 22)

  ! This indicates that dlsode exited improperly
  if (istate < 0) then
     dist = 0.
     return
  end if

  p(1) = y(1)
  p(2) = y(2)
  p(4) = t1


  ! keep track of new point
  r2=p(1)
  z2=p(2)
  phi2=p(4)
  pnew=(/r2,z2,phi2/)

  ! calculate distance between original point and new point
  ! accumulate distance and call connection length

  ! first, convert to cartesian so we can use the distance formula
  call pol2cart(pold,poldc)
  call pol2cart(pnew,pnewc)

  x1=poldc(1)
  y1=poldc(2)

  x2=pnewc(1)
  y2=pnewc(2)

  dist=sqrt((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)

  ! print *, dist

  return
end subroutine follow_field_gboozer

