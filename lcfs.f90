subroutine allocate_lcfs()

  use lcfs_module
  implicit none
  integer :: filenum = 22

  if (num_lcfs <= 0) return

  open(filenum, file=lcfs_file, status = 'old', form = 'formatted')
  read(filenum,*) lcfs_size(1:2)

  allocate(lcfs(lcfs_size(1), lcfs_size(2), 3))
  close(filenum)

end subroutine allocate_lcfs

subroutine load_lcfs()

  use lcfs_module
  implicit none

  character*72 :: dummy
  integer :: i,j
  real :: x,y,z
  integer :: filenum = 22

  if (num_lcfs <= 0) return

  open(filenum, file=lcfs_file, status = 'old', form = 'formatted')

  read(filenum, *) dummy

  do i=1, lcfs_size(1)
     do j=1, lcfs_size(2)
        read(filenum,*) lcfs(i,j,1:3)
     enddo
  enddo

  close(filenum)

end subroutine load_lcfs

real function distance_to_lcfs(rin, zin, phiin)

  use lcfs_module
  use coil_module

  implicit none

  integer :: i, mindex, index
  real, dimension(lcfs_size(2), 3) :: cut
  real, dimension(lcfs_size(2)) :: rlcfs, zlcfs, dist
  real :: pi, phi_extent, ratio, phi_step_size
  real :: rin, zin, phiin, r, z, phi
  real :: x1, x2, y1, y2, m, b

  if (num_lcfs <=0) then
     distance_to_lcfs = 0
     return
  end if

  pi = 3.1415927
  phi_extent = 2*pi / coil_sections / (is_mirrored + 1)

  call move_to_first_quad(rin, zin, phiin, r, z, phi, coil_sections, &
       is_mirrored)

  phi_step_size = phi_extent / (lcfs_size(1) - 1)

  ! ratio for interpolation
  ratio = modulo(phi, phi_step_size) / phi_step_size
  ! Index for first surface
  index = int(phi/phi_step_size) + 1
  
  ! Interpolate
  cut = lcfs(index,:,:) * (1 - ratio) + lcfs(index + 1,:,:) * ratio

  rlcfs = sqrt(cut(:,1)**2 + cut(:,2)**2)
  zlcfs = cut(:,3)

  ! calculate the distances
  dist = (r - rlcfs)**2 + (z - zlcfs)**2

  !what is minimum?
  mindex = minloc(dist, DIM=1)
  
 
  
  x1 = rlcfs(mindex)
  y1 = zlcfs(mindex)
  ! case where you are not on the end
  if (mindex > 1 .or. mindex < lcfs_size(2)) then
     if (dist(mindex + 1) > dist(mindex - 1)) then        
        x2 = rlcfs(mindex - 1)
        y2 = zlcfs(mindex - 1)
     else
        x2 = rlcfs(mindex + 1)
        y2 = zlcfs(mindex + 1)
     end if
  else
     if (dist(2) > dist(lcfs_size(2) - 1)) then
        x2 = rlcfs(lcfs_size(2) - 1)
        y2 = zlcfs(lcfs_size(2) - 1)
     else
        x2 = rlcfs(2)
        y2 = zlcfs(2)
     end if
  end if
  !write (*,*) 'phi_step_size',phi_step_size
  !write (*,*) 'phi, index ratio',phi, index, ratio
  !write (*,*) 'x1, y1, x2, y2',x1,y1,x2,y2


  ! now we've got the line points for the closest segment, so calculate the
  ! distance to that segment, line equation is y - mx - b = 0
  m = (y2 - y1) / (x2 - x1)
  b = y1 - (m * x1)
  

  !the equation for distance to a line is |y0 - mx0 - b|/sqrt(m^2 + 1)
  distance_to_lcfs = abs(z - m*r - b)/((m**2 + 1)**0.5)


end function distance_to_lcfs
      

