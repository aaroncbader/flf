module limiter_module

  implicit none
  integer, dimension(2) :: limiter_size
  integer :: num_limiters
  
  real, allocatable, dimension(:,:) :: limiter
  character*72 :: limiter_file

end module limiter_module
