! functions to check if the field line hits the limiter

integer function is_near_helical_plane(point,delta)
implicit none

real, dimension(3) :: bvector
real, dimension(3) :: baxis
real, dimension(3) :: dist_axis
real :: dist_plane
<<<<<<< HEAD
integer :: is_near_helical_plane
=======
>>>>>>> ebe160ad2127f7fed2b798c7e22c982a9a03d114
real :: delta
real, dimension(3) :: point

! use pre-calculated values of the location of the magnetic axis in QHS and the B field vector at this location

bvector=(/0.0,0.43765823,0.24171643/)
baxis=(/1.4454,0.0,0.0/)

! use the B field vector as the normal vector to the helical plane. Find the distance to this plane by using the dot product.
dist_axis=point-baxis
dist_plane=dot_product(dist_axis,bvector)

if (dist_plane < delta) then
   is_near_helical_plane=1
else
   is_near_helical_plane=0
end if

end function is_near_helical_plane



integer function hit_limiter(point,xlimiter,ylimiter,poly_size)
implicit none

real :: Xpoint, Ypoint
<<<<<<< HEAD
integer :: hit_limiter, in_polygon, poly_size
=======
integer :: in_polygon, poly_size
>>>>>>> ebe160ad2127f7fed2b798c7e22c982a9a03d114
real, dimension(3) :: bvector, baxis
real, dimension(poly_size) :: Xpoly, Ypoly, xlimiter, ylimiter
real, dimension(3) :: point, HC_out, HC_up, HC_up_norm

! use the same pre-calculated values for the normal vector
bvector=(/0.0,0.43765823,0.24171643/)

! need to project point into 2D plane
! we'll do this in the same way that Chris Clark does in his matlab scripts
! the 2d HC plane is defined by an 'out' coordinate: the x axis
! and an 'up' coordinate, defined by the y anc z coordinates together

HC_out=(/1.0,0.0,0.0/)

! compute the cross product between HC_out and bvector (the normal vector)
HC_up(1)=(HC_out(2)*bvector(3)-HC_out(3)*bvector(2))
HC_up(2)=(HC_out(3)*bvector(1)-HC_out(1)*bvector(3))
HC_up(3)=(HC_out(1)*bvector(2)-HC_out(2)*bvector(1))

! normalize HC_up
HC_up_norm=HC_up/(sqrt(sum(HC_up*HC_up)))

! find the dot product 
Xpoint= point(1) 
Ypoint= dot_product(point,HC_up) 

! now we can work within the 2d helical plane
! get 2d limiter coordinates
Xpoly=xlimiter
Ypoly=ylimiter

! use in_polygon to see if the point hits the limiter	
hit_limiter=in_polygon(Xpoint, Ypoint, Xpoly, Ypoly, poly_size)

end function hit_limiter
