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
  use coil_module

  implicit none
  integer :: tor_size, pol_size, index, i, in_polygon
  real, dimension(vessel_size(1)) :: phi_vessel
  real, dimension(vessel_size(2), 3) :: cut
  real, dimension(vessel_size(2)) :: rvessel, zvessel
  real :: rin, zin, phiin, phi_step_size, phi_extent
  real :: ratio, pi, r, z, phi

  ! Make sure there's even a vessel to check
  if (num_vessels.le.0) then
     inside_vessel = 1
     return
  end if

  pi = 3.1415927
  tor_size = vessel_size(1)
  pol_size = vessel_size(2)
  ! for now assume the vessel spans the same as the coil set
  phi_extent = 2 * pi /coil_sections

  call move_to_first_quad(rin, zin, phiin, r, z, phi, coil_sections, &
       is_mirrored)

  ! This is the step size in the toroidal direction
  phi_step_size = phi_extent/(tor_size - 1)

  ! \todo find a quicker assignment for fortran
  do i=1,tor_size
     phi_vessel(i) = real(i-1)
  enddo
  phi_vessel = phi_vessel / tor_size
  phi_vessel = phi_vessel * phi_extent + (2 * phi_step_size)
  phi_vessel = phi_vessel - phi_step_size



  ! ratio for interpolation
  ratio = modulo(phi, phi_step_size) / phi_step_size
  ! Index for first surface
  index = int((phi - modulo(phi, phi_step_size))/phi_step_size) + 1
  
  ! Interpolate
  cut = vessel(index,:,:) * (1 - ratio) + vessel(index + 1,:,:) * ratio

  ! convert to R and Z
  rvessel = sqrt(cut(:,1)**2 + cut(:,2)**2)
  zvessel = cut(:,3)

  inside_vessel = in_polygon(r, z, rvessel, zvessel, vessel_size(2))

  return

end function inside_vessel

! Xpoint and Ypoint are the X and Y coordinates of the point.
! Xpoly and Ypoly are two arrays with the X and Y coordinates
! for the polygons.
! poly_size is the number of points in the polygon

! the following is old fortran code adatped from the source: http://www.ecse.rpi.edu/~wrf/Research/Short_Notes/pnpoly.html
! it performs a check to see how many times a vertical line through the point passes through the polygon
integer function in_polygon(Xpoint, Ypoint, Xpoly, Ypoly, poly_size)

  implicit none
  integer :: poly_size, i, j
  real :: px, py, Xpoint, Ypoint, qq
  logical :: mx, my, nx, ny
  real, dimension(poly_size) :: Xpoly, Ypoly
  real, allocatable, dimension(:) :: xx, yy, x, y

  allocate(xx(poly_size))
  allocate(yy(poly_size))
  allocate(x(poly_size))
  allocate(y(poly_size))

  ! print *, 'in_polygon started'	
  
  xx=Xpoly
  yy=Ypoly
  
  px=Xpoint
  py=Ypoint
  
  do i=1,poly_size

     x(i)=xx(i)-px
     y(i)=yy(i)-py
     
  end do

in_polygon=-1

  do i=1,poly_size
     j=1+modulo(i,poly_size)
     mx=x(i).ge.0.0
     nx=x(j).ge.0.0
     my=y(i).ge.0.0
     ny=y(j).ge.0.0
     if(.not.((my.or.ny).and.(mx.or.nx)).or.(mx.and.nx)) then
       cycle
     end if
     
     if((my.and.ny.and.(mx.or.nx).and..not.(mx.and.nx))) then
       in_polygon = -in_polygon
       cycle
     end if
     
     qq = (y(i)*x(j)-x(i)*y(j))/(x(j)-x(i))

     if (qq.lt.0) then
       cycle
     else if (qq == 0) then
        ! This condition means that it landed on the surface exactly
        in_polygon=0
        return
     else
        in_polygon=-in_polygon
     end if
  end do

  if (in_polygon.lt.0) then
    in_polygon = 0
  end if

  deallocate(xx)
  deallocate(yy)
  deallocate(x)
  deallocate(y)
  
 ! print *, 'in_polygon=', in_polygon
  
 ! print *, 'in_polygon ended'


end function in_polygon
