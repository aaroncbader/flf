<<<<<<< HEAD
subroutine get_vessel_dimensions(vessel_file, vessel_size)
! hello world
=======
subroutine allocate_vessel()

  use vessel_module
>>>>>>> 4c034bc377e794e2333430c2f2592354198d80ea
  implicit none
  integer :: filenum = 21

  open(filenum, file=vessel_file, status='old', form = 'formatted')
  read(filenum,*) vessel_size(1:2)

  allocate(vessel(vessel_size(1),vessel_size(2),3))
  close(filenum)

end subroutine allocate_vessel

subroutine load_vessel()

  use vessel_module
  implicit none

  character*72 :: dummy
  integer :: i,j
  real :: x,y,z
  integer :: filenum = 21

  open(filenum,file=vessel_file,status='old',form='formatted')

  
  ! the first two value should give the number of toroidal and poloidal
  ! points respectively.
  read(filenum,*) dummy
  !print *,dummy

  do i=1,vessel_size(1)
     do j=1,vessel_size(2)
        read(filenum,*) vessel(i,j,1:3)
     enddo
  enddo
  close(filenum)

end subroutine load_vessel


! note, point is in r,z,phi
integer function inside_vessel(rin, zin, phiin)

  use vessel_module
  implicit none
  
  integer :: tor_size, pol_size, phi8th, index, i, in_polygon
  real, dimension(vessel_size(1)) :: phi_vessel
  real, dimension(vessel_size(2), 3) :: cut
  real, dimension(vessel_size(2)) :: rvessel, zvessel
  real :: rin, zin, phiin, phi_step_size
  real :: ratio, pi, r, z, phi

  pi = 3.141592
  tor_size = vessel_size(1)
  pol_size = vessel_size(2)

  r = rin
  z = zin
  phi = phiin

  ! Make phi between 0 and 2 pi
  phi = mod(phi, 2. * pi)
  ! Note which octant we're in
  phi8th = int(phi/(pi/4.))
  ! Move to correct octant
  phi = mod(phi, pi / 4.)

  ! Invoke stell symmetry
  if (mod(phi8th, 2).ne.0) then
     phi = pi/4 - phi
     z = -1.0 * z
  endif



  ! This is the step size in the toroidal direction
  phi_step_size = pi/(tor_size - 1)/4

  ! \todo find a quicker assignment for fortran
  do i=1,tor_size
     phi_vessel(i) = real(i-1)
  enddo
  phi_vessel = phi_vessel / tor_size
  phi_vessel = phi_vessel * pi/4 + (2 * phi_step_size)
  phi_vessel = phi_vessel - phi_step_size



  ! ratio for interpolation
  ratio = mod(phi, phi_step_size) / phi_step_size
  ! Index for first surface
  index = int((phi - mod(phi, phi_step_size))/phi_step_size) + 1
  
  ! Interpolate
  cut = vessel(index,:,:) * (1 - ratio) + vessel(index + 1,:,:) * ratio

  ! convert to R and Z
  rvessel = sqrt(cut(:,1)**2 + cut(:,2)**2)
  zvessel = cut(:,3)

  inside_vessel = in_polygon(r, z, rvessel, zvessel, vessel_size(2))

  return

end function inside_vessel


! Calculates Cauchy integral and determines whether a 
! point is inside the polygon or outside.  If inside
! it returns 1, if outside, 0.

! Xpoint and Ypoint are the X and Y coordinates of the point.
! Xpoly and Ypoly are two arrays with the X and Y coordinates
! for the polygons. 
! poly_size is the number of points in the polygon

integer function in_polygon(Xpoint, Ypoint, Xpoly, Ypoly, poly_size)

  implicit none
  
  integer :: poly_size
  real :: Xpoint, Ypoint
  real,dimension(poly_size) :: Xpoly, Ypoly, xaver, yaver, top1, top2, bottom
  real :: summation


  xaver = 0.5*(Xpoly + cshift(Xpoly,1)) - Xpoint
  yaver = 0.5*(Ypoly + cshift(Ypoly,1)) - Ypoint

  top1 = xaver * (cshift(Ypoly,1) - Ypoly)
  top2 = yaver * (cshift(Xpoly,1) - Xpoly)

  bottom = xaver**2 + yaver**2
  
  summation = sum((top1 - top2)/bottom)

  !print *,summation
  if ((summation > 5.7).and.(summation < 6.8)) then
     in_polygon = 1
  else
     in_polygon = 0
  endif

  return

end function in_polygon
