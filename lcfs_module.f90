module lcfs_module
  
  implicit none
  
  integer :: num_lcfs=0
  integer, dimension(2) :: lcfs_size

  real, allocatable, dimension(:, :, :) :: lcfs
  character*144 :: lcfs_file

end module lcfs_module
