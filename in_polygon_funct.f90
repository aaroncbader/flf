integer function in_polygon(Xpoint, Ypoint, Xpoly, Ypoly, poly_size)

  implicit none
  
  integer :: poly_size, in_polygon, i, j
  real :: px, py, Xpoint, Ypoint
  logical :: mx, my, nx, ny
  real, dimension(poly_size) :: xx, yy, x, y, Xpoly, Ypoly
  
  xx=Xpoly
  yy=Ypoly
  
  px=Xpoint
  py=Ypoint 
  
  do 1 i=1,poly_size
  	
  	x(i)=xx(i)-px
  1	y(i)=yy(i)-py
  	in_polygon=-1
  	
  	do 2 i=1,poly_size
  	j=1+mod(i,poly_size)
  	mx=x(i).ge.0.0
  	nx=x(j).ge.0.0
  	my=y(i).ge.0.0
  	ny=y(j).ge.0.0
  	if(.not.((my.or.ny).and.(mx.or.nx)).or.(mx.and.nx)) go to 2
  	if(.not.(my.and.ny.and.(mx.or.nx).and..not.(mx.and.nx))) go to 3
  	in_polygon=-in_polygon
  	go to 2
  3	if ((y(i)*x(j)-x(i)*y(j))/(x(j)-x(i))) 2,4,5
  4   in_polygon=0
  	return
  5   in_polygon=-in_polygon
  2   continue	
  
  print *, in_polygon

end function in_polygon
