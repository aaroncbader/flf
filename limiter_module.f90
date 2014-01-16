module limiter_module

  implicit none
  integer, dimension(2) :: limiter_size
  
  real, allocatable, dimension(:,:) :: limiter
  character*144 :: limiter_file

end module limiter_module
