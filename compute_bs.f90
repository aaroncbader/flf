! compute full field
subroutine compute_full_bs(p, b)

  implicit none

  real,dimension(3) :: p,b,btemp
  
  ! Initialize b field
  b =  0

  ! main fields
  call compute_bs(p, 0, btemp)
  b = b + btemp
  ! aux fields
  call compute_bs(p, 1, btemp)
  b = b + btemp

end subroutine compute_full_bs
  

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
real, dimension(:), allocatable :: xcoil, ycoil, zcoil 
real, dimension(:), allocatable :: xcoilshift, ycoilshift, zcoilshift
integer :: arggood
real :: ax, ay, az, bx, by, bz, cx, cy, cz
real :: cxax, cxay, cxaz, magc, current, magb
real :: w, adotc, adotb, cross_sq
integer :: coilnumber, numcoilpts, isaux
real, dimension(3) :: p, b, bseg
real :: mu0, pi

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
  real, dimension(neq) :: y, dydx, div_hit
  real, dimension(3) :: bxyz, pxyz, przphi
  real :: br, bphi, t

  przphi(1) = y(1)
  przphi(2) = y(2)
  przphi(3) = t

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
  else if (inside_div(y(1), y(2), t).gt.1) then
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
   else if (inside_limiter(y(1),y(2),t) == 1) then
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
  
  
