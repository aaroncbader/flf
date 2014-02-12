module limiter_module

  implicit none
  integer, dimension(2) :: limiter_size
  integer :: num_limiters
  
  real, allocatable, dimension(:,:) :: limiter
  character*72, dimension(:), allocatable :: lim_files

end module limiter_module
