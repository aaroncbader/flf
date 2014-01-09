module div_module

  implicit none
  
  integer :: div_number, axis_points

  integer, allocatable, dimension(:) :: div_tor_num, div_seg_num
  ! index for divertor is
  !divertor(divertor number, toroidal seg index, segment part index, 
  !   r or z)
  real, allocatable, dimension(:,:,:,:) :: divertor
  ! index for div_tor_vals is
  !div_tor_vals(divertor number, toroidal seg index)
  real, allocatable, dimension(:,:) :: div_tor_vals
  real, allocatable, dimension(:,:) :: mag_axis

end module
