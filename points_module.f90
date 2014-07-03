! This module will define allocatable arrays for use in
! bookkeeping for points
module points_module

  implicit none
  real, dimension(:,:), allocatable :: points_start, points_move, points_end
  real, dimension(:), allocatable :: conn_length
  integer, dimension(:), allocatable :: points_hit_vessel, points_hit_limiter
  integer, dimension(:), allocatable :: points_hit_divertor, points_hit, points_complete
  real :: points_dphi
  integer :: n_iter, points_number
  integer :: current_point
  character*72 :: points_file, results_file

end module points_module
