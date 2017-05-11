! compute full field
subroutine compute_full_bs(p, b)

  use options_module
  implicit none

  double precision,dimension(3) :: p,b,btemp
  
  ! Initialize b field
  b =  0
  
  !*** BIOT SAVART field calculation ***
  if (coil_type.eq.1) then

    ! main fields
    call compute_bs(p, 0, btemp)
    b = b + btemp
    ! aux fields
    call compute_bs(p, 1, btemp)
    b = b + btemp

  !*** Magnetic grid field calculation ***  
  else if (coil_type.eq.2) then
    call field_from_mgrid_cubic(p, b)
    !call field_from_mgrid_linear(p, b)
  end if
end subroutine compute_full_bs

subroutine field_from_mgrid_cubic(p,b)
  use coil_module
  use mgrid_module
  implicit none
  double precision,dimension(3) :: p,b,brzphi,temp,przphi

  call cart2pol(p, temp)
  call move_to_first_quad(temp(1), temp(2), temp(3), przphi(1), &
        przphi(2), przphi(3), num_periods, is_mirrored)
   
  !HACK, if the grid is slightly off you might be able to sneak
  !in between phi values, causing an error
  if (przphi(3) > mgrid_phimax) przphi(3) = mgrid_phimax - 0.00000000001
  !make sure we are within the bounds
  if ((przphi(1) < mgrid_rmin).or.(przphi(1) > mgrid_rmax).or.&
      (przphi(2) < mgrid_zmin).or.(przphi(2) > mgrid_zmax).or.&
      (przphi(3) < mgrid_phimin).or.(przphi(3) > mgrid_phimax)) then

      b = 0
      return
  end if

  call interp_single_variable_cubic(przphi,brzphi(3),3)
  call interp_single_variable_cubic(przphi,brzphi(2),2)
  call interp_single_variable_cubic(przphi,brzphi(1),1)

  b(1) = brzphi(1)*cos(temp(3)) - brzphi(3)*sin(temp(3)) 
  b(2) = brzphi(1)*sin(temp(3)) + brzphi(3)*cos(temp(3))
  b(3) = brzphi(2)

  !figure out if we're in a mirrored section
  if (temp(2).ne.(przphi(2))) then
    b(3) = -1*b(3)
  end if

end subroutine field_from_mgrid_cubic

subroutine interp_single_variable_cubic(p,b,bselect)
  use mgrid_module
  use coil_module
  implicit none

  double precision,dimension(3) :: p,temp
  integer :: ri1, zi1, pi1, bselect
  integer :: tri1, tzi1, tpi1
  !double precision,pointer,dimension(mgrid_nphi, mgrid_nz, mgrid_nr) :: bgrid
  double precision,pointer,dimension(:,:,:) :: bgrid
  double precision,dimension(4,4,4) :: bmat
  double precision,dimension(4) :: parr, zarr, rarr, u
  double precision :: pstep, b
  double precision, dimension(4,4) :: s


  if (bselect .eq. 3) then
    bgrid => mgrid_bphi
  else if (bselect .eq. 2) then
    bgrid => mgrid_bz
  else 
    bgrid => mgrid_br
  end if  

  call get_prz_indices(p, ri1, zi1, pi1)
  !if (pi1 < -1) then
  !  write (*,*) 'bad pi1:',p, pi1
  !end if

  ! handle low or high values of the indices by setting 
  ! temporary values which later get shifted
  if (ri1 .le. 1) then
    tri1 = 2
    rarr = mgrid_r(1:4)
  else if (ri1 .ge. mgrid_nr - 1) then
    tri1 = mgrid_nr - 2
    rarr = mgrid_r(mgrid_nr-3:mgrid_nr)
  else
    tri1 = ri1
    rarr = mgrid_r(ri1-1:ri1+2)
  endif

  if (zi1 .le. 1) then
    tzi1 = 2
    zarr = mgrid_z(1:4)
  else if (zi1 .ge. mgrid_nz - 1) then
    tzi1 = mgrid_nz - 2
    zarr = mgrid_z(mgrid_nz-3:mgrid_nz)
  else
    tzi1 = zi1
    zarr = mgrid_z(zi1-1:zi1+2)
  endif
  
  !now assign all 64 b values
  !deal with possible wrap-arounds in phi for the first index
  
  if (pi1 .eq. 1) then
    bmat(1,:,:) = bgrid(mgrid_nphi-1, tzi1-1:tzi1+2, tri1-1:tri1+2)
    !write(*,*) 'at beginning'
  else
    bmat(1,:,:) = bgrid(pi1-1, tzi1-1:tzi1+2, tri1-1:tri1+2)
  end if

  bmat(2:3,:,:) = bgrid(pi1:pi1+1, tzi1-1:tzi1+2, tri1-1:tri1+2)
  
  if (pi1 .eq. mgrid_nphi - 1) then
    bmat(4,:,:) = bgrid(2, tzi1-1:tzi1+2, tri1-1:tri1+2)
    !write(*,*) 'at end'
  else
    bmat(4,:,:) = bgrid(pi1+2, tzi1-1:tzi1+2, tri1-1:tri1+2)
  end if

  !make the phi array
  pstep = (mgrid_phi(mgrid_nphi) - mgrid_phi(1))/(mgrid_nphi - 1)
  if (pi1 .le. 1) then
    parr(1) = mgrid_phi(1) - pstep
    parr(2:4) = mgrid_phi(1:3)
  else if (pi1 .ge. mgrid_nphi - 1) then
    parr(1:3) = mgrid_phi(mgrid_nphi-2:mgrid_nphi)
    parr(4) = mgrid_phi(mgrid_nphi) + pstep
  else
    parr = mgrid_phi(pi1-1:pi1+2)
  end if
  !write(*,*) 'p',p
  !write(*,*) 'parr',parr
  !write(*,*) 'pi1',pi1
  !Finally we handle the situations described above where
  !we invoked a shifted temporary index for r and z
  !we give it a dummy value which is a huge hack and I 
  !hope doesn't come back to bite me....but it probably will
  if (ri1 .le. 1) then
    bmat(:,:,2:4) = bmat(:,:,1:3)
    bmat(:,:,1) = -1e20
  end if
  if (ri1 .ge. mgrid_nr - 1) then
    bmat(:,:,1:3) = bmat(:,:,2:4)
    bmat(:,:,4) = -1e20
  end if
  if (zi1 .le. 1) then
    bmat(:,2:4,:) = bmat(:,1:3,:)
    bmat(:,1,:) = -1e20
  end if
  if (zi1 .ge. mgrid_nz - 1) then
    bmat(:,1:3,:) = bmat(:,2:4,:)
    bmat(:,4,:) = -1e20
  end if
  !write(*,*) 'bmat',bmat(:,1,1)

  !ok we're all set, we have our bvectors now we need
  !to compute some points
  !phi interpolation
  call cubic_interp(p(3),s(1,1), parr, bmat(:,1,1))
  call cubic_interp(p(3),s(1,2), parr, bmat(:,1,2))
  call cubic_interp(p(3),s(1,3), parr, bmat(:,1,3))
  call cubic_interp(p(3),s(1,4), parr, bmat(:,1,4))
  call cubic_interp(p(3),s(2,1), parr, bmat(:,2,1))
  call cubic_interp(p(3),s(2,2), parr, bmat(:,2,2))
  call cubic_interp(p(3),s(2,3), parr, bmat(:,2,3))
  call cubic_interp(p(3),s(2,4), parr, bmat(:,2,4))
  call cubic_interp(p(3),s(3,1), parr, bmat(:,3,1))
  call cubic_interp(p(3),s(3,2), parr, bmat(:,3,2))
  call cubic_interp(p(3),s(3,3), parr, bmat(:,3,3))
  call cubic_interp(p(3),s(3,4), parr, bmat(:,3,4))
  call cubic_interp(p(3),s(4,1), parr, bmat(:,4,1))
  call cubic_interp(p(3),s(4,2), parr, bmat(:,4,2))
  call cubic_interp(p(3),s(4,3), parr, bmat(:,4,3))
  call cubic_interp(p(3),s(4,4), parr, bmat(:,4,4))
  
  !z interpolation
  call cubic_interp(p(2), u(1), zarr, s(:,1))
  call cubic_interp(p(2), u(2), zarr, s(:,2))
  call cubic_interp(p(2), u(3), zarr, s(:,3))
  call cubic_interp(p(2), u(4), zarr, s(:,4))
  
  !r interpolation
  
  call cubic_interp(p(1), b, rarr, u)

end subroutine interp_single_variable_cubic

!Do a cubic interpolation using four points,
! with the desired point pf, lying between p(2) and p(3)
!
! pf is the value of the point you want to find
! p is the point values for the four points surrounding it,
! two on each side
!
! b is the value of the interpolating quantity at the array
! values in p.
! bf is the output.
! This is not an optimized calculation...
subroutine cubic_interp(pf, bf, p, b)
  implicit none

  double precision :: t, pf, bf, m2, m3
  double precision, dimension(4) :: p, b
  
  !t is the relative distance between p(2) and p(3)
  t = (pf - p(2))/(p(3) - p(2))

  !m2 is the slope at point 2
  !huge hack here, use a large negative value to indicate that we are at the edge
  !and need a one sided derivative.
  if (b(1) < -1.E10) then
    ! In this case we need a one sided derivative
    m2 = (b(3) - b(2))/(p(3) - p(2))
  else
    !m2 = 0.5 * ((b(3) - b(2))/(p(3) - p(2)) + (b(2) - b(1))/(p(2) - p(1)))
    m2 = (b(3) - b(1))/(p(3) - p(1))
  end if

  if (b(4) < -1.E11) then
    m3 = (b(3) - b(2))/(p(3) - p(2))
  else
    !m3 is the slope at point 3
    m3 = (b(4) - b(2))/(p(4) - p(2))
    !m3 = 0.5 * ((b(4) - b(3))/(p(4) - p(3)) + (b(3) - b(2))/(p(3) - p(2)))
  end if

  !calculate the value
  bf = (1 + 2*t)*(1 - t)*(1 - t)*b(2)
  bf = bf + t*(1-t)*(1-t)*(p(3) - p(2))*m2
  !bf = bf + t*(1-t)*(1-t)*m2
  bf = bf + t*t*(3-2*t)*b(3)
  bf = bf + t*t*(t-1)*(p(3) - p(2))*m3
  !bf = bf + t*t*(t-1)*m3
end subroutine cubic_interp

!Calculate the field from a uniform magnetic grid in
! r z phi space, linear interpolation
subroutine field_from_mgrid_linear(p, b)
  use mgrid_module
  use coil_module
  implicit none

  double precision,dimension(3) :: p, b, przphi, temp
  double precision :: br1, br2, br3, br4, br5, br6, br7
  double precision :: bz1, bz2, bz3, bz4, bz5, bz6, bz7
  double precision :: bp1, bp2, bp3, bp4, bp5, bp6, bp7
  double precision :: rstep, zstep, phistep, rd, zd, phid
  integer :: ri1, ri2, zi1, zi2, pi1, pi2

  ! convert to rzphi
  call cart2pol(p, temp)
  call move_to_first_quad(temp(1), temp(2), temp(3), przphi(1), &
        przphi(2), przphi(3), num_periods, is_mirrored)
   
  !HACK, if the grid is slightly off you might be able to sneak
  !in between phi values, causing an error
  if (przphi(3) > mgrid_phimax) przphi(3) = mgrid_phimax - 0.00000000001
  !make sure we are within the bounds
  if ((przphi(1) < mgrid_rmin).or.(przphi(1) > mgrid_rmax).or.&
      (przphi(2) < mgrid_zmin).or.(przphi(2) > mgrid_zmax).or.&
      (przphi(3) < mgrid_phimin).or.(przphi(3) > mgrid_phimax)) then

      b = 0
      return
  end if

  call get_prz_indices(przphi, ri1, zi1, pi1)
  ri2 = ri1 + 1
  zi2 = zi1 + 1
  pi2 = pi1 + 1

  rd = (przphi(1) - mgrid_r(ri1))/(mgrid_r(ri2) - mgrid_r(ri1))
  zd = (przphi(2) - mgrid_z(zi1))/(mgrid_z(zi2) - mgrid_z(zi1))
  phid = (przphi(3) - mgrid_phi(pi1))/(mgrid_phi(pi2) - mgrid_phi(pi1))

  !Linear interpolate r
  br1 = mgrid_br(pi1, zi1, ri1)*(1-rd) + mgrid_br(pi1, zi1, ri2)*rd
  br2 = mgrid_br(pi1, zi2, ri1)*(1-rd) + mgrid_br(pi1, zi2, ri2)*rd
  br3 = mgrid_br(pi2, zi1, ri1)*(1-rd) + mgrid_br(pi2, zi1, ri2)*rd
  br4 = mgrid_br(pi2, zi2, ri1)*(1-rd) + mgrid_br(pi2, zi2, ri2)*rd
  bz1 = mgrid_bz(pi1, zi1, ri1)*(1-rd) + mgrid_bz(pi1, zi1, ri2)*rd
  bz2 = mgrid_bz(pi1, zi2, ri1)*(1-rd) + mgrid_bz(pi1, zi2, ri2)*rd
  bz3 = mgrid_bz(pi2, zi1, ri1)*(1-rd) + mgrid_bz(pi2, zi1, ri2)*rd
  bz4 = mgrid_bz(pi2, zi2, ri1)*(1-rd) + mgrid_bz(pi2, zi2, ri2)*rd
  bp1 = mgrid_bphi(pi1, zi1, ri1)*(1-rd) + mgrid_bphi(pi1, zi1, ri2)*rd
  bp2 = mgrid_bphi(pi1, zi2, ri1)*(1-rd) + mgrid_bphi(pi1, zi2, ri2)*rd
  bp3 = mgrid_bphi(pi2, zi1, ri1)*(1-rd) + mgrid_bphi(pi2, zi1, ri2)*rd
  bp4 = mgrid_bphi(pi2, zi2, ri1)*(1-rd) + mgrid_bphi(pi2, zi2, ri2)*rd

  !Linear interpolate z
  br5 = br1*(1-zd) + br2*zd
  br6 = br3*(1-zd) + br4*zd
  bz5 = bz1*(1-zd) + bz2*zd
  bz6 = bz3*(1-zd) + bz4*zd
  bp5 = bp1*(1-zd) + bp2*zd
  bp6 = bp3*(1-zd) + bp4*zd

  !Linear interpolate phi
  br7 = br5*(1-phid) + br6*phid
  bz7 = bz5*(1-phid) + bz6*phid
  bp7 = bp5*(1-phid) + bp6*phid

  b(1) = br7*cos(temp(3)) - bp7*sin(temp(3)) 
  b(2) = br7*sin(temp(3)) + bp7*cos(temp(3))
  b(3) = bz7
  !Check if in a mirrored section
  if (temp(2).ne.(przphi(2))) then
    b(3) = -1*b(3)
  end if

end subroutine field_from_mgrid_linear  


!Find the indices in the mgrid that give a point
! that precedes prz
subroutine get_prz_indices(przphi, ri1, zi1, pi1)
  use mgrid_module
  implicit none

  double precision,dimension(3) :: przphi
  integer :: ri1, zi1, pi1
  double precision :: rstep, zstep, phistep

  !The mgrids all have uniform grids, so we can cheat to determine
  !which indices are the appropriate ones

  rstep = (mgrid_r(mgrid_nr) - mgrid_r(1))/(mgrid_nr - 1)
  ri1 = floor((przphi(1) - mgrid_rmin)/rstep) + 1

  zstep = (mgrid_z(mgrid_nz) - mgrid_z(1))/(mgrid_nz - 1)
  zi1 = floor((przphi(2) - mgrid_zmin)/zstep) + 1

  phistep = (mgrid_phi(mgrid_nphi) - mgrid_phi(1))/(mgrid_nphi - 1)
  pi1 = floor((przphi(3) - mgrid_phimin)/phistep) + 1
  
end subroutine get_prz_indices

subroutine compute_bs(p, isaux, b)
      
      ! call this routine once for every point that is being followed
      ! use successive pairs of points in each coil to define a "current stick". 
      ! calculate the field due to each current stick for each coil for all points on the grid where the line is being followed.
      ! Inputs:
      ! p is the point to calculate in (x, y, z) in meters
      ! isaux is 1 if we are dealing with the aux coils and 0 for the main coils.
      ! bmag is the output, the magnitude of the b field
 
      ! r0- (x0,y0,z0) vector for first point in current stick
      ! r1- (x1,y1,z1) vector for second point in current stick
      ! r- (x,y,z) point where field line is being followed
      ! for bookkeeping, a=r1-r0, b=r0-r, c=r1-r
      ! when we find a, b, and c, we can use this equation to find the magnetic field:
      ! b_field=mu0*I*(c cross a)*((a dot c)/|c|-(a dot b)/|b|)/(4*pi*|(c cross a)|^2)
      
use coil_module

implicit none
    
integer :: i, j, k
double precision, dimension(:), allocatable :: xcoil, ycoil, zcoil 
double precision, dimension(:), allocatable :: xcoilshift, ycoilshift, zcoilshift
integer :: arggood
double precision :: ax, ay, az, bx, by, bz, cx, cy, cz
double precision :: cxax, cxay, cxaz, magc, current, magb
double precision :: w, adotc, adotb, cross_sq
integer :: coilnumber, numcoilpts, isaux
double precision, dimension(3) :: p, b, bseg
double precision :: mu0, pi

!p=(/0.1,0.1,0.1/)

b=0
if (isaux == 1) then  
   coilnumber = aux_count
else
   coilnumber = main_count
endif


mu0=1.25663706E-6 ! in mks
pi=3.14159265359
!for debugging
!mu0 = 1.0

!loop for each coil
do i=1,coilnumber
!do i=1,1

      
      if (isaux == 1) then
         numcoilpts = aux_points(i)
         current = aux_current(i)
      else
         numcoilpts = main_points(i)
         current = main_current(i)
      endif

      ! make sure we actually have a coil
      if (numcoilpts.le.1) then
         cycle
      endif
      

!     don't waste time with calculation if there's no current in this coil!
      if (current == 0) then

         cycle
      endif
   
      allocate(xcoil(numcoilpts))
      allocate(ycoil(numcoilpts))
      allocate(zcoil(numcoilpts))
      allocate(xcoilshift(numcoilpts))
      allocate(ycoilshift(numcoilpts))
      allocate(zcoilshift(numcoilpts))


      ! assign values to x, y, z arrays from the coil_module
      if (isaux == 1) then
        xcoil=coil_aux(i,1:numcoilpts,1)
        ycoil=coil_aux(i,1:numcoilpts,2)
        zcoil=coil_aux(i,1:numcoilpts,3)
      else
        xcoil=coil_main(i,1:numcoilpts,1)
        ycoil=coil_main(i,1:numcoilpts,2)
        zcoil=coil_main(i,1:numcoilpts,3)
      endif

      ! circularly shift coil point before the loop
      ! this step can be pre-calculated to save time, 
      ! probably doesn't save much though (AB)
      xcoilshift=cshift(xcoil,1)
      ycoilshift=cshift(ycoil,1)
      zcoilshift=cshift(zcoil,1)
     
            
      !loop over all points for each coil
      do j=1,numcoilpts-1
!      do j=1,10
        bseg = 0 
       
        ! subtract point of interest r vector from coil points           
        bx=xcoil(j)-p(1)
        by=ycoil(j)-p(2)
        bz=zcoil(j)-p(3)        

        ! subtract point of interest r vector from shifted coil points
        cx=xcoilshift(j)-p(1)
        cy=ycoilshift(j)-p(2)
        cz=zcoilshift(j)-p(3)

        ! now, subtract the unshifted difference from the shifted difference
        ax=cx-bx
        ay=cy-by
        az=cz-bz

        ! pull out NaN values, not sure why they're being created right now
        ! looks like Paul does a version of the same thing
        ! There shouldn't be any Nan values.  We need to track this down (AB)
        arggood = 1
        if (ax/=ax .or. ay/=ay .or. az/=az) then
           arggood=0
        end if

        ! use arggood to reset the loop if the current point is a NaN
        if (arggood==0) then
           cycle
        end if

        ! find cross products
        cxax=cy*az-cz*ay
        cxay=cz*ax-cx*az
        cxaz=cx*ay-cy*ax
        

        ! pick up by aaron
        ! This is a simple form where you compute dl cross R1 / R1^3
        ! The other form should be slightly more accurate, but this is
        ! faster.

        ! magc = ((cx*cx) + (cy*cy) + (cz*cz))**1.5
       
        ! bseg(1) = cxax
        ! bseg(2) = cxay
        ! bseg(3) = cxaz
        ! bseg = bseg*mu0*current/(4*pi*magc)
        ! b = b + bseg

        cross_sq = (cxax*cxax + cxay*cxay + cxaz*cxaz)
        
        adotc = ax*cx + ay*cy + az*cz
        adotb = ax*bx + ay*by + az*bz
        magc = sqrt(cx*cx + cy*cy + cz*cz)
        magb = sqrt(bx*bx + by*by + bz*bz)
        w = adotc/magc - adotb/magb
        bseg(1) = cxax * w/cross_sq * current * mu0/4/pi
        bseg(2) = cxay * w/cross_sq * current * mu0/4/pi
        bseg(3) = cxaz * w/cross_sq * current * mu0/4/pi

        b = b + bseg
        
     enddo

     deallocate(xcoil)
     deallocate(ycoil)
     deallocate(zcoil)
     deallocate(xcoilshift)
     deallocate(ycoilshift)
     deallocate(zcoilshift)


   
      
end do  
       


end subroutine compute_bs

! calculates the gradient at a point, need to figure
! out an appropriate stepsize, for now use a characteristic
! value used for mgrid spacing of 2 mm
! the first index of db is the B component, the second index
! is the derivative direction
subroutine gradb(pxyz, db)
  implicit none

  double precision, dimension(3) :: pxyz, bxyz1, bxyz2
  double precision, dimension(3,3) :: db
  double precision :: dx
  
  !This is the step size
  dx = 0.002
  
  !x-direction
  call compute_full_bs(pxyz + (/ dx, dble(0.0), dble(0.0) /), bxyz1)
  call compute_full_bs(pxyz - (/ dx, dble(0.0), dble(0.0) /), bxyz2)

  db(:,1) = (bxyz1 - bxyz2)/(2*dx)


  !y-direction
  call compute_full_bs(pxyz + (/ dble(0.0), dx, dble(0.0) /), bxyz1)
  call compute_full_bs(pxyz - (/ dble(0.0), dx, dble(0.0) /), bxyz2)

  db(:,2) = (bxyz1 - bxyz2)/(2*dx)

  !z-direction
  call compute_full_bs(pxyz + (/ dble(0.0), dble(0.0), dx /), bxyz1)
  call compute_full_bs(pxyz - (/ dble(0.0), dble(0.0), dx /), bxyz2)

  db(:,3) = (bxyz1 - bxyz2)/(2*dx)
  
  
  return
end subroutine gradb
  


! Like the field_deriv function but integrates along path length
! y is a 3 value vector of (x, y, z)
subroutine field_deriv_s(neq, t, y, dydx)
  use points_module
  implicit none

  integer :: neq
  double precision, dimension(neq) :: y, dydx
  double precision, dimension(3) :: bxyz, pxyz
  double precision :: bmag, t
  ! Probably not necessary, but helpful to reassign for bookkeeping
  pxyz(1) = y(1)
  pxyz(2) = y(2)
  pxyz(3) = y(3)

  call compute_full_bs(pxyz, bxyz)
  bmag = sqrt(bxyz(1)*bxyz(1) + bxyz(2)*bxyz(2) + bxyz(3)*bxyz(3))
  dydx(1) = bxyz(1)/bmag
  dydx(2) = bxyz(2)/bmag
  dydx(3) = bxyz(3)/bmag
  return 
end subroutine field_deriv_s

!like field_deriv_s, but also calculates the psi values
subroutine field_deriv_s_wpsi(neq, t, y, dydx)
  use points_module
  implicit none

  integer :: neq
  double precision, dimension(neq) :: y, dydx
  double precision, dimension(3) :: bxyz, pxyz, psixyz
  double precision, dimension(3,3) :: db
  double precision :: bmag, t
  ! Probably not necessary, but helpful to reassign for bookkeeping
  pxyz(1) = y(1)
  pxyz(2) = y(2)
  pxyz(3) = y(3)

  call compute_full_bs(pxyz, bxyz)
  bmag = sqrt(bxyz(1)*bxyz(1) + bxyz(2)*bxyz(2) + bxyz(3)*bxyz(3))
  dydx(1) = bxyz(1)/bmag
  dydx(2) = bxyz(2)/bmag
  dydx(3) = bxyz(3)/bmag
  !get the derivatives
  call gradb(pxyz, db)
  dydx(4) = (-1./bmag)*(db(1,1)*y(4) + db(2,1)*y(5) + db(3,1)*y(6))
  dydx(5) = (-1./bmag)*(db(1,2)*y(4) + db(2,2)*y(5) + db(3,2)*y(6))
  dydx(6) = (-1./bmag)*(db(1,3)*y(4) + db(2,3)*y(5) + db(3,3)*y(6))
  return 
end subroutine field_deriv_s_wpsi
  

  

! This is a function to be called from dlsode to compute the field derivatives
! It takes values of r,z,phi, converts to x,y,z, calculates the field,
! converts back to r,z,phi, and calculates the derivatives dr/dphi and dz/dphi

! neq is the number of variables (in this case 2)
! t is the independent variable (in our case, phi)
! y is the dependent variables (r and z)
! dydx are the derivatives (dr/dphi and dz/dphi)

subroutine field_deriv(neq, t, y, dydx)
  
  use points_module
  implicit none

  integer :: neq, inside_vessel, inside_div, inside_limiter
  double precision, dimension(neq) :: y, dydx, div_hit
  double precision, dimension(3) :: bxyz, pxyz, przphi
  double precision :: br, bphi, t

  przphi(1) = y(1)
  przphi(2) = y(2)
  przphi(3) = t
  !write(*,*) 'phi: ',t


  ! we already hit, so no point in calculating (this is a sanity check)
  if (points_hit(current_point).eq.1) then
     dydx = 0
     return
  end if
  
  ! print *, 'points_end=', points_end

  ! For all these checks, if there is no object loaded,
  ! then it immediately leaves the function without
  ! doing anything

  ! check if we've hit the wall 
  !\todo refactor this
  if ((inside_vessel(y(1), y(2), t) == 0)) then
     points_hit_vessel(current_point) = 1
     points_hit(current_point) = 1
     points_end(current_point,1:2) = y
     points_end(current_point,3) = t
     print *,'-------------------------------'
     print *,'current point at wall:',current_point
     print *,points_end(current_point,:)
     print *,'-------------------------------'
     dydx = 0
   ! check if we've hit the divertor
  else if (inside_div(y(1), y(2), t).ge.1) then
    ! this is inefficient, but we only need to recalc once per point
    points_hit_divertor(current_point) = inside_div(y(1), y(2), t)
    points_hit(current_point) = 1
    points_end(current_point,1:2) = y
    points_end(current_point,3) = t
    print *,'-------------------------------'
    print *,'current point at divertor:',current_point
    print *,points_end(current_point,:)
    print *,'-------------------------------'
    dydx = 0
   ! check if we're near the helical plane in the boxport (where the limiter is)  
   ! if we start them at the limiter
   else if (inside_limiter(y(1), y(2), t) .ge. 1) then
     points_hit_limiter(current_point)=1
     points_hit(current_point) = 1
     points_end(current_point,1:2)=y
     points_end(current_point,3)=t
     print *,'-------------------------------'
     print *,'current point at limiter:',current_point
     print *,points_end(current_point,:)
     print *,'-------------------------------'
     dydx = 0
   	  
     return
  end if
     

  ! convert to cartesian
  call pol2cart(przphi, pxyz)
  ! compute field
  call compute_full_bs(pxyz, bxyz)
  ! compute br and bphi
  br = bxyz(1)*cos(t) + bxyz(2)*sin(t)
  bphi = -bxyz(1)*sin(t) + bxyz(2)*cos(t)
  ! compute dydx
  ! write (*,'(6(F10.7,2X))'),pxyz(:),bxyz(:)
  ! write (*,'(3(F10.7,2X))'),y(:),t
  dydx(1) = y(1) * br/bphi
  dydx(2) = y(1) * bxyz(3)/bphi
  return
end subroutine field_deriv
  
  
