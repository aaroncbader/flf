program comprehensive_test
  use points_module
  use coil_module

  integer :: i,j

  !Run some tests to make sure everything is correct
  !with loading and other stuff
  
  !Make sure the loaded file is the sample input file
  call read_input()
  call get_points()
  
  !Turn off the limiter for now
  num_limiters = 0
  points_dphi = 1.0
  current_point = 1

  
  points_start(1,1) = 1.45
  points_start(1,2) = 0.
  points_start(1,3) = 0.

  points_move(1,:) = points_start(1,:)
  !write (*,'(3(F10.7,2X))'),points_move(1,:)
  call follow_field(points_move(1,:), points_dphi)
  !write (*,'(3(F10.7,2X))'),points_move(1,:)
  if ((points_move(1,1).gt.1.09181).and.(points_move(1,1).lt.(1.09183)).and.&
      (points_move(1,2).gt.(-0.10317)).and.(points_move(1,2).lt.(-0.10316)))&
      then
     print *,'test with no auxiliary current PASS'
  else
     print *,'test with no auxiliary current FAIL'
  end if
  
  !add some aux currents to the mix, assumes we loaded 6 coils
  do i=1,6
     taper(i) = (0.02*i)
     do j=0,7
        aux_current(i + (j*6)) = main_current(1)*14 * 0.02*i
        
     end do
  end do

  points_move(1,:) = points_start(1,:)
  call follow_field(points_move(1,:), points_dphi)
  if ((points_move(1,1).gt.1.09810).and.(points_move(1,1).lt.(1.09811)).and.&
      (points_move(1,2).gt.(-0.09564)).and.(points_move(1,2).lt.(-0.09563)))&
      then
     print *,'test with auxiliary current PASS'
  else
     print *,'test with auxiliary current FAIL'
  end if
  !write (*,'(3(F10.7,2X))'),points_move(1,:)

end program comprehensive_test
