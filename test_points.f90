program test_points
  use points_module
  call get_points
  print *,points_number, points_dphi
  print *,points_start(2,:)
  print *,points_move(3,:)
  points_hit(1) = 1
  print *,points_hit(1)
end program test_points
