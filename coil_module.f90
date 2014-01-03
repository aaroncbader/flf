module coil_module

   implicit none

   !integer, parameter :: main_size = 1000, aux_size = 100
   !integer, parameter :: main_count = 48, aux_count = 48, taper_size = 6

   integer :: main_size, aux_size, main_count, aux_count, taper_size, skip
   real, allocatable, dimension(:,:,:) :: coil_main, coil_aux
   real, allocatable, dimension(:) :: main_current, aux_current
   integer, allocatable, dimension(:) :: main_points, aux_points


end module coil_module
