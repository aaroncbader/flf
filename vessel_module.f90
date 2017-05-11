module vessel_module

  implicit none

  integer :: num_vessels=0
  integer, dimension(2) :: vessel_size
  
  double precision, allocatable, dimension(:, :, :) :: vessel
  character*144 :: vessel_file=''

end module vessel_module
