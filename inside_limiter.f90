subroutine allocate_limiter()

  use limiter_module
  implicit none
  integer :: filenum = 22
  integer :: i, max_size


  if (num_limiters == 0) return
  
  ! print *, 'number of limiters loaded:',num_limiters

  max_size = 0
  ! Read through all limiter files and set allocation values
  allocate(limiter_size(num_limiters))
  allocate(lim_bvector(num_limiters, 3))
  allocate(lim_baxis(num_limiters, 3))

  do i=1,num_limiters

     open(filenum, file=trim(lim_files(i)), status='old', form = 'formatted')
     read(filenum,*) limiter_size(i)
     if (limiter_size(i) > max_size) max_size = limiter_size(i)
     close(filenum)
  end do

  ! Now allocate all the limiters	

  allocate(limiter(num_limiters, max_size, 2))

  ! Initialize
  limiter(:,:,:)=0

  close(filenum)



end subroutine allocate_limiter

subroutine load_limiter()

  use limiter_module
  implicit none

  integer :: i,j, dum1
  integer :: filenum = 22
  double precision, dimension(2) :: dummy

  do i=1,num_limiters

     open(filenum,file=trim(lim_files(i)),status='old',form='formatted')
     

     ! the first two value should give the number of toroidal and poloidal
     ! points respectively.
     read(filenum,*) dum1
     read(filenum,*) lim_bvector(i,:)
     read(filenum,*) lim_baxis(i,:)


     !do i=1,limiter_size(1)
     do j=1,limiter_size(i)
        read(filenum,*) dummy
        limiter(i,j,:) = dummy
     enddo
     !enddo
     close(filenum)

  end do  

end subroutine load_limiter


! note, point is in r,z,phi
integer function inside_limiter(r, z, phi)

  use limiter_module
  use points_module

  implicit none

  double precision :: Xpoint, Ypoint, dist_plane, delta, r, z, phi, phi_mod, phi_mod2, pi
  integer :: in_polygon, poly_size, is_near_helical_plane, i
  double precision, dimension(3) :: dist_axis, lim_rzp
  double precision, dimension(:), allocatable :: Xpoly, Ypoly
  double precision, dimension(3) :: point, pointc, HC_out, HC_up, HC_up_mag, HC_up_norm


  if (num_limiters.le.0) then
     inside_limiter = 0
     return
  end if

  pi = 3.1415927
  point=(/r,z,phi/)

  ! print *, 'point in limiter=', point

  call pol2cart(point,pointc)

  ! Only do the calculation if you are less than the step number
  if (lim_minstep .gt. current_step) then
     is_near_helical_plane = 0
     inside_limiter = 0
     return
  end if
  

  do i=1,num_limiters
     
     ! Only do the calculation if you are less than the step number
     
     allocate(Xpoly(limiter_size(i)))
     allocate(Ypoly(limiter_size(i)))

     ! Coarse check for phi distance (the detailed check with dot-products can
     ! produce errors when 180 degrees off)

     ! Convert to cartesian
     call cart2pol(lim_baxis(i,:), lim_rzp)

     ! Get everything between 0 and 2pi
     lim_rzp(3) = modulo(lim_rzp(3), 2 * pi)
     phi_mod = modulo(point(3), 2* pi)
     if (lim_rzp(3) > phi_mod) then
        phi_mod2 = phi_mod + (2 * pi)
     else
        phi_mod2 = phi_mod - (2 * pi)
     end if

     !print *,'stuff',phi_mod, phi_mod2, lim_rzp(3)
     ! We check if it's 1 radian away.  If greater, then skip this check,
     ! we do two checks to avoid the annoying wrap around at 0, 2*pi
     if ((abs(lim_rzp(3) - phi_mod) > 1.0) .and. &
          (abs(lim_rzp(3) - phi_mod2) > 1.0)) then
        is_near_helical_plane=0
        inside_limiter=0
        deallocate(Xpoly)
        deallocate(Ypoly)
        cycle
     endif
     
     !print *, 'point is closer than 1 radian'

     ! use the B field vector as the normal vector to the helical plane. Find
     ! the distance to this plane by using the dot product.

     dist_axis = pointc - lim_baxis(i,:)
	   !dist_axis = abs(pointc - lim_baxis(i,:))
     dist_plane = dot_product(dist_axis, lim_bvector(i,:))
     !print *,'phi',point(3)
     !print *,'pointc',pointc(:)
     !print *,'dist_axis',dist_axis(:)
     !print *,'dist_plane',dist_plane


     ! Check if we're too far away
     if (abs(dist_plane) > lim_halfwidth) then
        is_near_helical_plane=0
        inside_limiter=0
        deallocate(Xpoly)
        deallocate(Ypoly)
        !print *,'too far away', dist_plane
        cycle
     endif

     !print *, 'performing limiter check'

     poly_size=limiter_size(i)

      Xpoly=limiter(i,:,1)
      Ypoly=limiter(i,:,2)

     ! need to project point into 2D plane we'll do this in the same way
     ! that Chris Clark does in his matlab scripts the 2d HC plane is
     ! defined by an 'out' coordinate: the x axis and an 'up'
     ! coordinate, defined by the y anc z coordinates together

     HC_out=(/1.0,0.0,0.0/)

     ! compute the cross product between HC_out and lim_bvector (the
     ! normal vector)
     HC_up(1)=(HC_out(2)*lim_bvector(i,3)-HC_out(3)*lim_bvector(i,2))
     HC_up(2)=(HC_out(3)*lim_bvector(i,1)-HC_out(1)*lim_bvector(i,3))
     HC_up(3)=(HC_out(1)*lim_bvector(i,2)-HC_out(2)*lim_bvector(i,1))

     ! normalize HC_up

     HC_up_mag=sqrt(HC_up(1)**2 + HC_up(2)**2 + HC_up(3)**2)	

     HC_up_norm=HC_up/HC_up_mag


     !print *, HC_up_norm

     ! find the dot product 
     Xpoint= pointc(1) 

     Ypoint= dot_product(pointc,HC_up_norm) 
	 
	 !print *, "Xpoint=", Xpoint
	 !print *, "Ypoint=", Ypoint
	 !print *, "Xpoly=", Xpoly
	 !print *, "Ypoly=", Ypoly


     ! now we can work within the 2d helical plane
     ! use in_polygon to see if the point hits the limiter	
     !print *,Xpoint, Ypoint

     inside_limiter=in_polygon(Xpoint, Ypoint, Xpoly, Ypoly, poly_size)
	 !print *, "first inside_limiter= ", inside_limiter	  

     deallocate(Xpoly)
     deallocate(Ypoly)


     if (inside_limiter == 1) exit

     !print *, 'inside_limiter=', inside_limiter
     

  end do


end function inside_limiter



