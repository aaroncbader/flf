! convert rzphi points to xyz
subroutine pol2cart(pol, cart)

  implicit none
  real,dimension(3) :: pol, cart

  cart(1) = pol(1)*cos(pol(3))
  cart(2) = pol(1)*sin(pol(3))
  cart(3) = pol(2)

end subroutine pol2cart

!Takes r,z,phi in any quadrant and moves them to the first quadrant

!Technically nothing happens to the r coordinate, so this can be eliminated
! But for completeness it is included.
subroutine move_to_first_quad(r,z,phi,newr,newz,newphi)
  real :: r,z,phi,newr,newz,newphi,pi
  integer :: phi8th

  pi = 3.1415927
  
  ! Make phi between 0 and 2 pi
  newphi = modulo(phi, 2. * pi)

  ! Note which octant we're in
  phi8th = int(newphi/(pi/4.))  

  ! Move to correct octant
  newphi = modulo(newphi, pi / 4.)

  ! Invoke stell symmetry
  if (modulo(phi8th, 2).ne.0) then
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


! find intersection between a line from (x1, y1) to (x2, y2) and a line from
! (x3, y3) to (x4, y4).  The intersection value is saved in xint and yint.
! The function returns 1 if there is an intersection and 0 if there isn't. In
! these cases the values in xint and yint should *NOT* be trusted.  I could set
! it to NaN but that seems bad I guess.

integer function intersection(x1, y1, x2, y2, x3, y3, x4, y4, xint, yint)

  real :: x1, x2, x3, x4, y1, y2, y3, y4
  real :: xint, yint, m1, m3, b1, b3

  ! Before doing any calculations we check if any of the intersections are
  ! even possible.
  if (((x1.lt.x3).and.(x1.lt.x4).and.(x2.lt.x3).and.(x2.lt.x4)) .or. &
      ((x1.gt.x3).and.(x1.gt.x4).and.(x2.gt.x3).and.(x2.gt.x4)) .or. &
      ((y1.lt.y3).and.(y1.lt.y4).and.(y2.lt.y3).and.(y2.lt.y4)) .or. &
      ((y1.gt.y3).and.(y1.gt.y4).and.(y2.gt.y3).and.(y2.gt.y4))) then
     ! If any of these 4 conditions are satisfied it means all the points in
     ! one segment are less than or greater than all the points in the other
     ! segment for at least one of the coordinates.  No intersections are
     ! possible.
     xint = 0
     yint = 0
     intersection = 0
     return
  end if
  ! Slopes and y intercepts for the two segments
  m1 = (y2 - y1)/(x2 - x1)
  m3 = (y4 - y3)/(x4 - x3)
  b1 = y1 - m1 * x1
  b3 = y3 - m3 * x3

  ! Solve double equations y = m1*x + b1 and y = m2*x + b2 
  xint = (b3 - b1)/(m1 - m3)
  yint = m1 * xint + b1


  ! In order to be a valid intersection, the value xint must be in between the
  ! two values.  This mess checks that xint lies between x1 and x2 *and*
  ! between x3 and x4.  It does the same for y as well.  If all are satisfied,
  ! it's a valid intersection point.
  if (((xint.ge.x1).and.(xint.le.x2).or.(xint.ge.x2).and.(xint.le.x1)).and.&
      ((xint.ge.x3).and.(xint.le.x4).or.(xint.ge.x4).and.(xint.le.x3)).and.&
      ((yint.ge.y1).and.(yint.le.y2).or.(yint.ge.y2).and.(yint.le.y1)).and.&
      ((yint.ge.y3).and.(yint.le.y4).or.(yint.ge.y4).and.(yint.le.y3))) then
     intersection = 1
     return
  else
     xint = 0
     yint = 0
     intersection = 0
     return
  end if

end function intersection

! Compute cross product of 2 three dimensional vectors
subroutine cross_product(a, b, c)
  implicit none
  real, dimension(3) :: a, b, c

  c(1) = a(2) * b(3) - a(3) * b(2)
  c(2) = a(3) * b(1) - a(1) * b(3)
  c(3) = a(1) * b(2) - a(2) * b(1)
  return
end subroutine cross_product
  
