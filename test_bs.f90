program test_bs
  use coil_module
  implicit none
  real, dimension(1) :: b
  integer :: i

  call read_coils()

! set current
  do i = 1,main_count
     coil_set%main_current(i) = -150105./14
  enddo

  call compute_bs((/1.45,0.,0./), 0, b)
  print *,b
end program test_bs
