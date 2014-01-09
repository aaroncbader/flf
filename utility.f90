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
