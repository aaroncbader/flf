! This module will define allocatable arrays for use in
! bookkeeping for points
module points_module

  implicit none
  real, dimension(:,:), allocatable :: points_start, points_move
  real, dimension(:), allocatable :: points_hit
  real :: points_dphi
  integer :: n_iter, points_number

end module points_module
