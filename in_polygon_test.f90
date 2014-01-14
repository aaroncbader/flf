program in_polygon_test

  implicit none
  
  integer :: poly_size, in_polygon
  real :: Xpoint, Ypoint
  real,dimension(4) :: Xpoly, Ypoly
  real,dimension(3) :: xaver, yaver, top1, top2, bottom
  real :: summation

  Xpoly=(/0.0,10.0,10.0,0.0/)
  Ypoly=(/0.0,0.0,10.0,10.0/)
  
  Xpoint=3
  Ypoint=3
  
  poly_size=4
  

  xaver = 0.5*(Xpoly(2:4) + Xpoly(1:3)) - Xpoint
  yaver = 0.5*(Ypoly(2:4) + Ypoly(1:3)) - Ypoint
  
  print *, xaver
  print *, yaver

  top1 = xaver * (Ypoly(2:4)-Ypoly(1:3))
  top2 = yaver * (Xpoly(2:4)-Xpoly(1:3))
  
  print *, top1
  print *, top2

  bottom = xaver*xaver + yaver*yaver
  
  summation = sum((top1 - top2)/bottom)

  print *,summation
  
  if ((summation > 5.7).and.(summation < 6.8)) then
     in_polygon = 1
  else
     in_polygon = 0
  endif
  
  print *,in_polygon

end program in_polygon_test
