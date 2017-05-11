module mgrid_module

  implicit none
  
  character*72 :: mgrid_file
  integer :: mgrid_nr, mgrid_nz, mgrid_nphi
  double precision :: mgrid_phimin, mgrid_phimax, mgrid_rmin, mgrid_rmax
  double precision :: mgrid_zmin, mgrid_zmax
  double precision, allocatable, dimension(:) :: mgrid_r, mgrid_z, mgrid_phi
  double precision, target, allocatable, dimension(:,:,:) :: mgrid_br, mgrid_bz, mgrid_bphi
end module mgrid_module
