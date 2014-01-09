!Takes r,z,phi in any quadrant and moves them to the first quadrant

!Technically nothing happens to the r coordinate, so this can be eliminated
! But for completeness it is included.
subroutine move_to_first_quad(r,z,phi,newr,newz,newphi)
  real :: r,z,phi,newr,newz,newphi,pi
  integer :: phi8th

  pi = 3.1415927
  
  ! Make phi between 0 and 2 pi
  newphi = mod(phi, 2. * pi)

  ! Note which octant we're in
  phi8th = int(newphi/(pi/4.))  

  ! Move to correct octant
  newphi = mod(newphi, pi / 4.)

  ! Invoke stell symmetry
  if (mod(phi8th, 2).ne.0) then
     newphi = pi/4 - newphi
     newz = -1.0 * z
  else
     newz = z
  endif

  ! r never changes
  newr = r

end subroutine move_to_first_quad

! Given a function f(x) = y, find the value f(x0) by interpolation
! x must be in increasing order, but need not be evenly spaced

! numpoints is the size of the array
real function linear_interpolate(x0, y, x, numpoints) 

  implicit none
  integer :: i,numpoints
  real, dimension(numpoints) :: y, x
  real :: x1, x2, y1, y2, x0

  ! Handle out of bounds on the low end  
  if (x0 .le. x(1)) then
     x1 = x(1)
     x2 = x(2)
     y1 = y(1)
     y2 = y(2)
     print *,'test1'
  else if (x0 .ge. x(numpoints)) then
     x1 = x(numpoints - 1)
     x2 = x(numpoints)
     y1 = y(numpoints -1)
     y2 = y(numpoints)
     print *,'test2'
  else
     do i = 2,numpoints
        if (x0 .le. x(i)) then
           x1 = x(i-1)
           x2 = x(i)
           y1 = y(i-1)
           y2 = y(i)
           print *,x1,x2,y1,y2
           exit
        end if
        if (i .eq. numpoints) then
           !something bad happened here, this should not be possible
           print *,'Error in linear interpolation'
        end if
     end do
  end if

  linear_interpolate = (y2 - y1) * (x0 - x1)/(x2 - x1) + y1

end function linear_interpolate
