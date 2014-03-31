! program to write magnetic field magnitude

program Bmag

  use points_module
  use options_module

  implicit none
  integer :: outfile, j
  real :: magb
  real, dimension(3) :: b, pxyz


  call read_input()
  call get_points()
  
  outfile = 1
  open (unit=outfile, file=trim(adjustl(results_file)), status='unknown')

  do j=1,points_number
     call pol2cart(points_start(j,:), pxyz)
     call compute_full_bs(pxyz, b)
     magb = (b(1)**2 + b(2)**2 + b(3)**2)**0.5
     !print *,b, magb
     write(outfile, '(F11.6)') magb
  end do

end program Bmag
