module mgrid_module

  implicit none
  
  character*72 :: mgrid_file
  integer :: mgrid_nr, mgrid_nz, mgrid_nphi
  real :: mgrid_phimin, mgrid_phimax, mgrid_rmin, mgrid_rmax
  real :: mgrid_zmin, mgrid_zmax
  real, allocatable, dimension(:) :: mgrid_r, mgrid_z, mgrid_phi
  real, allocatable, dimension(:,:,:) :: mgrid_br, mgrid_bz, mgrid_bphi
end module mgrid_module
