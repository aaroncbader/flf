program test_limiter

implicit none

	real, dimension(3) :: point
	integer :: near, hit, delta
	integer :: is_near_helical_plane, hit_limiter, poly_size
	real, dimension(5) :: xlimiter, ylimiter ! don't forget to change this when the dimension of the limiter polygon changes

	point=(/1.39,-0.09715,0.1759/) ! in 3d cartesian, units (m)
	
	xlimiter=(/0,2,2,0,0/) ! x and y limiter coordinates in helical plane (hence 2d), units (m)
	ylimiter=(/-2,-2,2,2,-2/)
	poly_size=size(xlimiter)
	
	delta=0.1 ! proximity to helical plane

	near=is_near_helical_plane(point,delta)
	
	if (near == 1) then
		hit=hit_limiter(point,xlimiter,ylimiter,poly_size)
				
	end if	
	
	if (near == 1 .and. hit ==1) then
		print *, 'The point was near the helical plane and hit the polygon'
	else if (near == 1 .and. hit /= 1) then
		print *, 'The point was near the helical plane, but did not hit the polygon'
	else if (near == 0) then
		print *, 'The point was not near the helical plane'		
	end if	
	
end program test_limiter

