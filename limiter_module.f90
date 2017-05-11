module limiter_module

  implicit none
  integer, allocatable, dimension(:) :: limiter_size, lim_minstep
  integer :: num_limiters
  
  double precision, allocatable, dimension(:) :: lim_inside
  double precision, allocatable, dimension(:,:,:) :: limiter
  double precision, allocatable, dimension(:,:) :: lim_bvector, lim_baxis
  character*72, dimension(:), allocatable :: lim_files

end module limiter_module
