! p is the point in rzphi
! dphi is the distance in phi to follow

subroutine follow_field(p, dphi, dist, step_number)

  use coil_module


  parameter (lrw=58)
  parameter (liw=22)
  parameter (itask=1)

  real, dimension(3) :: p, pold, pnew, poldc, pnew
  real, dimension(2) :: y
  ! For now this is unused.
  real :: dphi, t0, t1, tol
  ! dstuff for dlsode
  integer :: ifail, istate, step_number
  real, dimension(liw) :: iwork
  real, dimension(lrw) :: rwork
  real, dimension(1) :: dist, r1, z1, phi1, r2, z2, phi2, x1, x2, y1, y2

  external field_deriv

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
