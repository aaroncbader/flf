module div_module

  implicit none
  
  integer :: num_divertors=0
  integer :: axis_points

  integer, allocatable, dimension(:) :: div_tor_num, div_seg_num, div_repeat
  ! index for divertor is
  !divertor(divertor number, toroidal seg index, segment part index, 
  !   r or z)
  double precision, allocatable, dimension(:,:,:,:) :: divertor
  ! index for div_tor_vals is
  !div_tor_vals(divertor number, toroidal seg index)
  double precision, allocatable, dimension(:,:) :: div_tor_vals
  double precision, allocatable, dimension(:,:) :: mag_axis
  character*72, dimension(:), allocatable :: div_files
  character*72 :: axis_file=''

end module
