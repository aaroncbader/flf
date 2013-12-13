module coil_module

   implicit none

   integer, parameter :: main_size = 1000, aux_size = 100
   integer, parameter :: main_count = 48, aux_count = 48, taper_size = 6


         ! Set up coil type definition
   type coils
      real, dimension(main_count, main_size, 3) :: main
      real, dimension(aux_count, aux_size, 3) :: aux
      integer, dimension(main_count) :: main_points
      integer, dimension(aux_count) :: aux_points
      real, dimension(main_count) :: main_current
      real, dimension(aux_count) :: aux_current
   end type coils

end module coil_module
