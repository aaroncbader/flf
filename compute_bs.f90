subroutine compute_bs(p, isaux, bmag)
      
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
    
integer :: i, j
real, dimension(:), allocatable :: xcoil, ycoil, zcoil 
real, dimension(:), allocatable :: xcoilshift, ycoilshift, zcoilshift
integer :: arggood
real :: ax, ay, az, bx, by, bz, cx, cy, cz
real :: cxax, cxay, cxaz, magcxa, magc, magb
real :: adotc, adotb, cxasq, csq, bsq, f2c
real :: sumcxf, sumcyf, sumczf, current
integer :: coilnumber, numcoilpts, isaux
real, dimension(3) :: p
real, dimension(1) :: blocal, bmag
real :: mu0, pi

!p=(/0.1,0.1,0.1/)
blocal=0
bmag=0
if (isaux == 1) then  
   coilnumber = aux_count
else
   coilnumber = main_count
endif

mu0=1.25663706E-6 ! in mks
pi=3.14159265359

!loop for each coil
do i=1,coilnumber
      
      if (isaux == 1) then
         numcoilpts = coil_set%aux_points(i)
         current = coil_set%aux_current(i)
      else
         numcoilpts = coil_set%main_points(i)
         current = coil_set%main_current(i)
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
            
      !loop over all points for each coil
      do j=1,numcoilpts
      
      	! assign values to x, y, z arrays from the coil_module
        if (isaux == 1) then
          xcoil(j)=coil_set%aux(i,j,1)
          ycoil(j)=coil_set%aux(i,j,2)
          zcoil(j)=coil_set%aux(i,j,3)
      	else
          xcoil(j)=coil_set%main(i,j,1)
          ycoil(j)=coil_set%main(i,j,2)
          zcoil(j)=coil_set%main(i,j,3)
        endif

      	! circularly shift coil point before the loop      
      	xcoilshift=cshift(xcoil,1)
      	ycoilshift=cshift(ycoil,1)
      	zcoilshift=cshift(zcoil,1)

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

            ! find dot products
            adotc=ax*cx+ay*cy+az*cz
            adotb=ax*bx+ay*by+az*bz
            
            ! find other, intermediate parts of the expression for b
            cxasq=cxax*cxax+ cxay*cxay+ cxaz*cxaz
            magcxa=sqrt(cxasq)
            csq=cx*cx+cy*cy+cz*cz
            magc=sqrt(cxasq)
            bsq=bx*bx+by*by+bz*bz
            magb=sqrt(bsq)
            f2c=(adotc/magc-adotb/magb)/(cxasq*((4*pi)**2))
                  
            ! find intermediate sums
            sumcxf=sumcxf+cxax*f2c
            sumcyf=sumcyf+cxay*f2c
            sumczf=sumczf+cxaz*f2c
                  
            ! find b for the coil
            blocal=blocal + mu0*(current)*(sumcxf*sumcyf*sumczf)
         
            end do
            
	! add all values of b for each coil
      bmag=bmag+blocal
      
      deallocate(xcoil)
      deallocate(ycoil)
      deallocate(zcoil)
      deallocate(xcoilshift)
      deallocate(ycoilshift)
      deallocate(zcoilshift)
      
end do  
       
print *, bmag

end subroutine compute_bs
