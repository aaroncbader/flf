program test_bs
  use coil_module
  implicit none
  real, dimension(3) :: b
  integer :: i


  call allocate_main(6,14)
  call allocate_aux('aux_c.dat')
  call read_coil_files(-10000)

! set current
  do i = 1,main_count
     main_current(i) = -150105./14
  enddo

  do i = 1,6
     aux_current(i) = 0.1 * (-150105)
  end do

  do i = 7,aux_count
     aux_current(i) = 0.
  end do

! This is for testing purposes!  It works, I think
!  call make_simple_coil()

  call compute_full_bs((/0.777817,0.777817,.3/), b)
  print *,b

  call deallocate_coils()

end program test_bs


! make a circular coil in the x-z plane centered at (1,0,0) with radius
! of 0.5
subroutine make_simple_coil()
  use coil_module
  implicit none

  integer :: i, coil_points
  real :: theta, pi, center, radius, mu0

  mu0 = 1.25663706E-6
  pi = 3.14159
  coil_points = 51
  radius = 0.5
  center = 1.0

  do i=0,coil_points-1
     theta = 2*pi*i/(coil_points-1)
     coil_main(1,i+1,1) = 0.0
     coil_main(1,i+1,2) = center + radius*cos(theta)
     coil_main(1,i+1,3) = radius*sin(theta)
!     print *,theta, cos(theta), main(1,i,1)
  enddo

  main_points(1) = coil_points
  main_current(1) = 1.0/mu0


  do i=2,main_count
     main_points(i) = 0
     main_current(i) = 0.0

  enddo


end subroutine make_simple_coil
  
