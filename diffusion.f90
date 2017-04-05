! needed functions


! get random vector perp to field vector
! p is the point in space
subroutine get_perp_vec(p, perp_vec)
  implicit none
  real, dimension(3) :: p, b, perp_vec, rand_vec
  real :: vec_size

  call compute_full_bs(p, b)
  call rand_vector(rand_vec)
  call cross_product(b, rand_vec, perp_vec)

  !write (*,*) 'pxyz', p
  !write (*,*) 'b', b
  !write (*,*) 'perp vec', perp_vec

  ! Normalize the vector
  vec_size = (perp_vec(1)**2 + perp_vec(2)**2 + perp_vec(3)**2)**0.5
  perp_vec = perp_vec/vec_size
  return
end subroutine get_perp_vec

! calculate diffusion time step
! LC is the connection length
subroutine get_diff_time(Lc, Te, species, tau)
  implicit none
  real :: Lc, Te, mass, q, tau
  integer :: species

  if (species .eq. 1) then
     mass = 9.10928E-31
  else
     mass = 1.67262E-28
  end if

  q = 1.60218E-19

  tau = Lc / (2 * Te * q/mass)**0.5
  
  return
end subroutine get_diff_time

subroutine get_varD(pxyz, D, Dnew)
  use options_module, only: varDgamma, varDB0
  implicit none
  real, dimension(3) :: pxyz, b
  real :: D, Dnew, magb

  call compute_full_bs(pxyz, b)
  magb = (b(1)**2 + b(2)**2 + b(3)**2)**0.5
  Dnew = D*(1.0 + varDgamma*(magb/varDB0 - 1.0))
  
end subroutine get_varD
  
! get the distance to diffuse a point
subroutine get_diff_distance(tau, D, pxyz, dist)
  use options_module, only: varD
  implicit none
  real, dimension(3) :: pxyz
  real :: tau, D, dist, Dnew

  if (varD == 1) then
     call get_varD(pxyz, D, Dnew)
  else
     Dnew = D
  end if
  dist = 2 * (D * tau)**0.5
  return
end subroutine get_diff_distance

!diffuse a point
subroutine diffuse_point(p, newp, Lc, Te, D, species)
  implicit none
  real, dimension(3) :: newp, p, perp_vec, pxyz 
  real :: Lc, Te, tau, dist, D 
  integer :: species

  !print *,'LC',Lc

  call pol2cart(p, pxyz)

  call get_perp_vec(pxyz, perp_vec)
  !print *,'perp vec',perp_vec
  
  call get_diff_time(Lc, Te, species, tau)
  !print *,'diff time',tau

  call get_diff_distance(tau, D, pxyz, dist)
  !print *,'diff dist',dist

  newp = pxyz + dist * perp_vec
  !print *,'oldp',p

  call cart2pol(newp, p)
  newp = p
  !print *,'move',dist * perp_vec
  !print *,'newp',newp
  return
end subroutine diffuse_point

subroutine diffuse_boozer(p, newp, step)
  use coil_module
  use div_module
  implicit none

  real, dimension(3) :: p, newp
  real :: phi, r, z, axis_phi, pi, step
  real :: rmag, zmag, rline, zline, magline
  real :: linear_interpolate
  integer :: axis_flip, axis_index, interp_index


  phi = p(3)
  r = p(1)
  z = p(2)

  call move_to_first_quad(r, z, phi, r, z, phi, &
        num_periods, is_mirrored)

  !We move stuff into the first quadrant to do the magnetic axis
  !calculation. But we need to save whether we're in a mirrored
  ! section, in order to properly move the point back after
  ! we're done with the diffusing
  pi = 3.14159265
  axis_flip = 1
  axis_phi = phi
  ! This is the test whether we are in a mirrored section
  if (z .ne. p(2)) then
     axis_flip = -1
  end if

  axis_index = interp_index(axis_phi, mag_axis(:,3), axis_points)


   rmag = linear_interpolate(axis_phi, mag_axis(:,1), mag_axis(:,3), &
         axis_index)

   rmag = rmag * axis_flip
   zmag = linear_interpolate(axis_phi, mag_axis(:,2), mag_axis(:,3), &
         axis_index)
   zmag = zmag * axis_flip

   rline = r - rmag
   zline = z - zmag
   magline = sqrt(rline**2 + zline**2)
   rline = rline/magline*step
   zline = zline/magline*step
   !write (*,*) r, z, rmag, zmag, rline, zline

   !Move the point
   newp(1) = r + rline
   !write(*,*) axis_flip
   newp(2) = (z + zline) * axis_flip
   newp(3) = p(3)
   !write(*,*) 'Point moved to'
end subroutine
