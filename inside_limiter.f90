subroutine allocate_limiter()

  use limiter_module
  implicit none
  integer :: filenum = 21

  open(filenum, file=limiter_file, status='old', form = 'formatted')
  read(filenum,*) limiter_size(1:2)

  allocate(limiter(limiter_size(1),limiter_size(2),3))
  close(filenum)

end subroutine allocate_limiter

subroutine load_limiter()

  use limiter_module
  implicit none

  character*72 :: dummy
  integer :: i,j
  integer :: filenum = 21

  open(filenum,file=limiter_file,status='old',form='formatted')

  
  ! the first two value should give the number of toroidal and poloidal
  ! points respectively.
  read(filenum,*) dummy
  !print *,dummy

  do i=1,limiter_size(1)
     do j=1,limiter_size(2)
        read(filenum,*) limiter(i,j)
     enddo
  enddo
  close(filenum)

end subroutine load_limiter


! note, point is in r,z,phi
integer function inside_limiter(point)

use limiter_module

implicit none

real :: Xpoint, Ypoint
integer :: in_polygon, poly_size
real, dimension(3) :: bvector, baxis
real, dimension(:) :: Xpoly, Ypoly
real, dimension(3) :: point, pointc, HC_out, HC_up, HC_up_norm

! convert point from rzphi to xyz
  call pol2cart(point,pointc)

! use the same pre-calculated values for the normal vector
bvector=(/0.0,0.43765823,0.24171643/)

! need to project point into 2D plane
! we'll do this in the same way that Chris Clark does in his matlab scripts
! the 2d HC plane is defined by an 'out' coordinate: the x axis
! and an 'up' coordinate, defined by the y anc z coordinates together

HC_out=(/1.0,0.0,0.0/)

! compute the cross product between HC_out and bvector (the normal vector)
HC_up(1)=(HC_out(2)*bvector(3)-HC_out(3)*bvector(2))
HC_up(2)=(HC_out(3)*bvector(1)-HC_out(1)*bvector(3))
HC_up(3)=(HC_out(1)*bvector(2)-HC_out(2)*bvector(1))

! normalize HC_up
HC_up_norm=HC_up/(sqrt(sum(HC_up*HC_up)))

! find the dot product 
Xpoint= pointc(1) 
Ypoint= dot_product(pointc,HC_up) 

! now we can work within the 2d helical plane
! get 2d limiter coordinates
Xpoly=limiter(:,1)
Ypoly=limiter(:,2)
poly_size=size(Xpoly)

! use in_polygon to see if the point hits the limiter	
inside_limiter=in_polygon(Xpoint, Ypoint, Xpoly, Ypoly, poly_size)

end function inside_limiter



