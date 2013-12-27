!test program
!program test_follow_field
subroutine test_follow_field
  implicit none
  real,dimension(3) :: p
  real :: dphi

  p(1) = 1.44
  p(2) = 0.
  p(3) = 0.
  dphi = 0.2
  print *,'start point rzphi'
  print *,p(1:3)

  call follow_field(p,dphi)
  print *,'end point rzphi'
  print *,p(1:3)
!end program test_follow_field
end subroutine test_follow_field


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

  ! first load the coils
  call read_coils()

  ! put current in main coils
  do i = 1,main_count
     coil_set%main_current(i) = -150105./14
  enddo

  do i = 1,aux_count
     coil_set%aux_current(i) = -150105*0.1
  enddo

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
