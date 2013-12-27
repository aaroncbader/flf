! test program to follow one point until it hits the wall

program follow_to_wall
  
  use coil_module
  real,allocatable,dimension(:,:,:) :: vessel
  integer,dimension(2) :: vessel_size
  real,dimension(3) :: p
  character*144 :: vessel_file
  integer :: i,j,isin, inside_vessel, n_iter
  real :: dphi

  ! first load the coils
  call read_coils()

  ! put current in main coils
  do i = 1,main_count
     coil_set%main_current(i) = -150105./14
  enddo

  do i = 1,aux_count
     coil_set%aux_current(i) = -150105*0.00000
  enddo

  ! load the vessel
  vessel_file = 'vessel.txt'
  call get_vessel_dimensions(vessel_file, vessel_size)
  allocate(vessel(vessel_size(1), vessel_size(2), 3))
  call load_vessel(vessel_file, vessel, vessel_size)

  ! set up the point
  p(1) = 1.36!r value
  p(2) = 0.27!z value
  p(3) = 0.  !phi value
  dphi = 0.1 ! value to go on one iteration

  niter = 100 !maximum number of iterations before giving up

  !file to write output
  open (unit=1,file='results.out',status='unknown')
  write (1,'(3(F9.6,2X))') p(1:3)

  

  do i=1,niter
     !check point to see if it's in the vessel
     isin = inside_vessel(p(1),p(2),p(3),vessel,vessel_size)
     
     if (isin == 0) then
       !point is not in, get out of the function
       write (1,*),'Hit at previous point'
       return
     endif

     call follow_field(p, dphi)
     
     ! write the new point
     write (1,'(3(F9.6,2X))') p(1:3)
  enddo

  !check the last point
  isin = inside_vessel(p(1),p(2),p(3),vessel,vessel_size)
     
  if (isin == 0) then
     !point is not in, get out of the function
     write (1,*),'Hit at previous point'
     return
  endif

  write(1,*),'Point did not hit vessel'

end program follow_to_wall

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
