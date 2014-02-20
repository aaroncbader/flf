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
  
! get the distance to diffuse a point
subroutine get_diff_distance(tau, D, dist)
  implicit none
  real, dimension(3) :: p
  real :: tau, D, dist

  dist = 2 * (D * tau)**0.5
  return
end subroutine get_diff_distance

!diffuse a point
subroutine diffuse_point(p, newp, Lc, Te, D, species)
  implicit none
  real, dimension(3) :: newp, p, perp_vec 
  real :: Lc, Te, tau, dist, D 
  integer :: species

  !print *,'LC',Lc

  call get_perp_vec(p, perp_vec)
  !print *,'perp vec',perp_vec
  
  call get_diff_time(Lc, Te, species, tau)
  !print *,'diff time',tau

  call get_diff_distance(tau, D, dist)
  !print *,'diff dist',dist

  newp = p + dist * perp_vec
  !print *,'oldp',p
  !print *,'move',dist * perp_vec
  !print *,'newp',newp
  return
end subroutine diffuse_point
  
