module limiter_module

  implicit none
  integer, allocatable, dimension(:) :: limiter_size, lim_minstep
  integer :: num_limiters
  
  real, allocatable, dimension(:) :: lim_inside
  real, allocatable, dimension(:,:,:) :: limiter
  real, allocatable, dimension(:,:) :: lim_bvector, lim_baxis
  character*72, dimension(:), allocatable :: lim_files

end module limiter_module
