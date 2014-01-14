subroutine allocate_vessel()

  use vessel_module
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
  
  integer :: tor_size, pol_size, index, i, in_polygon
  real, dimension(vessel_size(1)) :: phi_vessel
  real, dimension(vessel_size(2), 3) :: cut
  real, dimension(vessel_size(2)) :: rvessel, zvessel
  real :: rin, zin, phiin, phi_step_size
  real :: ratio, pi, r, z, phi

  pi = 3.1415927
  tor_size = vessel_size(1)
  pol_size = vessel_size(2)

  call move_to_first_quad(rin, zin, phiin, r, z, phi)

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

! the following is old fortran code adatped from the source: http://www.ecse.rpi.edu/~wrf/Research/Short_Notes/pnpoly.html
! it performs a check to see how many times a vertical line through the point passes through the polygon

integer function in_polygon(Xpoint, Ypoint, Xpoly, Ypoly, poly_size)

  implicit none
  
  integer :: poly_size, in_polygon, i, j
  real :: px, py, Xpoint, Ypoint
  logical :: mx, my, nx, ny
  real, dimension(poly_size) :: xx, yy, x, y, Xpoly, Ypoly
  
  xx=Xpoly
  yy=Ypoly
  
  px=Xpoint
  py=Ypoint 
  
  do 1 i=1,poly_size
  	
  	x(i)=xx(i)-px
  1	y(i)=yy(i)-py
  	in_polygon=-1
  	
  	do 2 i=1,poly_size
  	j=1+mod(i,poly_size)
  	mx=x(i).ge.0.0
  	nx=x(j).ge.0.0
  	my=y(i).ge.0.0
  	ny=y(j).ge.0.0
  	if(.not.((my.or.ny).and.(mx.or.nx)).or.(mx.and.nx)) go to 2
  	if(.not.(my.and.ny.and.(mx.or.nx).and..not.(mx.and.nx))) go to 3
  	in_polygon=-in_polygon
  	go to 2
  3	if ((y(i)*x(j)-x(i)*y(j))/(x(j)-x(i))) 2,4,5
  4   in_polygon=0
  	return
  5   in_polygon=-in_polygon
  2   continue	

end function in_polygon
