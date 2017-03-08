module options_module

  implicit none

  integer :: coil_type
  integer :: general_option = 1
  integer :: use_diffusion=0
  integer :: diffusion_species=0
  integer :: output_coils=0
  real :: dpar1, dpar2
  real :: d_perp, temperature, boozer_step, boozer_phi


end module options_module
