subroutine get_vessel_dimensions(vessel_file, vessel_size)

  implicit none
  character*144 :: vessel_file
  integer,dimension(2) :: vessel_size
  integer :: filenum = 21

  open(filenum, file=vessel_file, status='old', form = 'formatted')
  read(filenum,*) vessel_size(1:2)
  close(filenum)

end subroutine get_vessel_dimensions

subroutine load_vessel(vessel_file, vessel, vessel_size)
  implicit none

  integer, dimension(2) :: vessel_size
  character*144 :: vessel_file
  character*72 :: dummy
  integer :: vessel_tor, vessel_pol, i,j
  real,dimension(vessel_size(1),vessel_size(2),3) :: vessel
  real :: x,y,z
  integer :: filenum = 21

  open(filenum,file=vessel_file,status='old',form='formatted')

  
  ! the first two value should give the number of toroidal and poloidal
  ! points respectively.
  read(filenum,*) dummy
  print *,dummy

  do i=1,vessel_size(1)
     do j=1,vessel_size(2)
!        print *,i,j
        read(filenum,*) vessel(i,j,1:3)
     enddo
  enddo
  close(filenum)

end subroutine load_vessel


! note, point is in r,z,phi
integer function inside_vessel(rin, zin, phiin, vessel, vessel_size)

  implicit none
  
  integer, dimension(2) :: vessel_size
  integer :: tor_size, pol_size, phi8th, index, i
  real, dimension(vessel_size(1), vessel_size(2), 3) :: vessel
  real, dimension(vessel_size(1)) :: phi_vessel
  real, dimension(vessel_size(2), 3) :: cut
  real, dimension(vessel_size(2)) :: rvessel, zvessel, raver, zaver
  real, dimension(vessel_size(2)) :: top1, top2, bottom
  real :: rin, zin, phiin, phi_step_size
  real :: ratio, summation
  real :: pi, r, z, phi

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

  ! This is where I stop being able to follow stuff
  raver = 0.5*(rvessel + cshift(rvessel,1)) - r
  zaver = 0.5*(zvessel + cshift(zvessel,1)) - z

  top1 = raver * (cshift(zvessel,1) - zvessel)
  top2 = zaver * (cshift(rvessel,1) - rvessel)

  bottom = raver**2 + zaver**2
  
  summation = sum((top1 - top2)/bottom)

  !print *,summation
  if ((summation > 5.7).and.(summation < 6.8)) then
     inside_vessel = 1
  else
     inside_vessel = 0
  endif
  return

end function inside_vessel
