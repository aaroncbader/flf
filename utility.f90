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


! Get the index for interpolation.  If x0 is below all values of x, then
! the index is the first one.  If it's above, then it's the second from last.
! Otherwise the value is the largest value of x such that x0 > x

integer function interp_index(x0, x, numpoints)

  integer :: numpoints, i
  real :: x0
  real, dimension(numpoints) :: x

  if (x0 .le. x(1)) then
     interp_index = 1
     return
  else if (x0 .ge. x(numpoints)) then
     interp_index = numpoints - 1
     return
  else
     do i = 2,numpoints
        if (x0 .le. x(i)) then
           interp_index = i-1
           return
        end if
     end do
  end if
  print *,'Something went wrong in finding the interpolation index'
  interp_index = -1
  return

end function interp_index



! this function interpolates given the slope between x and y at the 
! values of ind and ind+1.  It does not check that the value is actually
! between the two indices, so be careful.
real function linear_interpolate(x0, y, x, ind)

  implicit none
  integer :: ind
  real, dimension(*) :: y, x
  real :: x0, x1, y1, x2, y2

  x1 = x(ind)
  y1 = y(ind)
  x2 = x(ind+1)
  y2 = y(ind+1)

  linear_interpolate = (y2 - y1) * (x0 - x1)/(x2 - x1) + y1

end function linear_interpolate



! Given a function f(x) = y, find the value f(x0) by interpolation
! x must be in increasing order, but need not be evenly spaced

! numpoints is the size of the array
real function linear_interpolate_full(x0, y, x, numpoints) 

  implicit none
  integer :: numpoints,interp_index, ind
  real, dimension(numpoints) :: y, x
  real :: x0, linear_interpolate

  ind = interp_index(x0, x, numpoints)

  linear_interpolate_full = linear_interpolate(x0, y, x, ind)

end function linear_interpolate_full
