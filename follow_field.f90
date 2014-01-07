! p is the point in rzphi
! dphi is the distance in phi to follow

subroutine follow_field(p, dphi)

  use coil_module


  parameter (lrw=58)
  parameter (liw=22)
  parameter (itask=1)

  real, dimension(3) :: p
  real, dimension(2) :: y
  ! For now this is unused.
  real, dimension(6) :: taper !may want this to be variable, we'll see.
  real :: dphi, t0, t1, tol
  ! dstuff for dlsode
  integer :: ifail, istate
  real, dimension(liw) :: iwork
  real, dimension(lrw) :: rwork

  external field_deriv

  y(1) = p(1)
  y(2) = p(2)

  ! start and stop phi values
  t0 = p(3)
  t1 = p(3) + dphi

  tol = 1.e-8

  ifail = 0
  istate = 1

  call dlsode(field_deriv, 2, y, t0, t1, 1, tol, tol, itask, istate, 0, &
       rwork, lrw, iwork, liw, jacl, 22)

  p(1) = y(1)
  p(2) = y(2)
  p(3) = t1

  return
end subroutine follow_field
