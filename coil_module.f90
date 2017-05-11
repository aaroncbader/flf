module coil_module

   implicit none

   !integer, parameter :: main_size = 1000, aux_size = 100
   !integer, parameter :: main_count = 48, aux_count = 48, taper_size = 6

   ! aux_count is equal to num_aux_coils * 8.
   integer :: num_periods=4, is_mirrored=0, main_winding=1
   integer :: num_main_coils, skip_value=1
   integer :: main_size, aux_size, main_count, aux_count, taper_size, aux_flag
   double precision, allocatable, dimension(:,:,:) :: coil_main, coil_aux
   double precision, allocatable, dimension(:) :: main_current, aux_current, taper
   double precision, dimension(:), allocatable :: aux_percent
   integer, allocatable, dimension(:) :: main_points, aux_points
   integer :: num_aux_coils = 0
   integer :: main_current_repeat = 0
   character*72 :: aux_file = ''
   character*72 :: coil_file_input
   character*72, dimension(:), allocatable :: main_files


end module coil_module
