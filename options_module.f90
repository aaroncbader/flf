module options_module

  implicit none

  integer :: coil_type
  integer :: general_option = 1
  integer :: use_diffusion=0
  integer :: diffusion_species=0
  integer :: output_coils=0, varD=0
  real :: dpar1, dpar2
  real :: d_perp, temperature, boozer_step, boozer_phi
  real :: varDgamma=1.0, varDB0=1.0


end module options_module
