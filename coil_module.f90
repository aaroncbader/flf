module coil_module

   implicit none

   !integer, parameter :: main_size = 1000, aux_size = 100
   !integer, parameter :: main_count = 48, aux_count = 48, taper_size = 6

   ! aux_count is equal to num_aux_coils * 8.
   integer :: coil_sections, is_mirrored, main_winding
   integer :: num_main_coils, num_aux_coils, skip_value
   integer :: main_size, aux_size, main_count, aux_count, taper_size
   real, allocatable, dimension(:,:,:) :: coil_main, coil_aux
   real, allocatable, dimension(:) :: main_current, aux_current, taper
   integer, allocatable, dimension(:) :: main_points, aux_points
   character*72 :: aux_file
   character*72, dimension(:), allocatable :: main_files


end module coil_module
