program comprehensive_test
  use points_module
  use coil_module
  use div_module

  integer :: i,j, inside_div, isok, answer
  real :: dist, D, Te
  real, dimension(3) :: newp

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
  call follow_field(points_move(1,:), points_dphi, dist)
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
  call follow_field(points_move(1,:), points_dphi, dist)
  if ((points_move(1,1).gt.1.09810).and.(points_move(1,1).lt.(1.09811)).and.&
      (points_move(1,2).gt.(-0.09564)).and.(points_move(1,2).lt.(-0.09563)))&
      then
     print *,'test with auxiliary current PASS'
  else
     print *,'test with auxiliary current FAIL'
  end if
  !write (*,'(3(F10.7,2X))'),points_move(1,:)


  !DIVERTOR TEST, setup
  num_divertors = 1
  call deallocate_div_and_axis()
  allocate(div_files(num_divertors))
  div_files(1) = 'samp_div.dat'
  axis_file = 'mag_axis.dat'
  call alloc_div()
  call load_div()
  call load_axis()

  ! test 1
  points_start(1,3) = 0.5498
  points_start(1,1) = 1.14
  isok = 1
  do i = 0,9
     points_start(1,2) = 0.17 + i*0.01
     answer = inside_div(points_start(1,1),&
          points_start(1,2), points_start(1,3))
     if ((points_start(1,2).lt.0.215).and.(answer.eq.1)) isok = 0
     if ((points_start(1,2).ge.0.215).and.(answer.eq.0)) isok = 0
     
  end do
  if (isok.eq.0) then
     print *,'Divertor test 1 FAIL'
  else
     print *,'Divertor test 1 PASS'
  end if


  points_start(1,3) = 1.0210
  points_start(1,1) = 1.14
  isok = 1
  do i = 0,9
     points_start(1,2) = -0.17 - i*0.01
     answer = inside_div(points_start(1,1),&
          points_start(1,2), points_start(1,3))
     
     if ((points_start(1,2).gt.-0.215).and.(answer.eq.1)) isok = 0
     if ((points_start(1,2).lt.-0.215).and.(answer.eq.0)) isok = 0
     
  end do
  if (isok.eq.0) then
     print *,'Divertor test 2 FAIL'
  else
     print *,'Divertor test 2 PASS'
  end if
  

  !! tests for diffusion
  points_start(1,1) = 1.45
  points_start(1,2) = 0.
  points_start(1,3) = 0.
  call init_random_seed()
  Te = 50.
  D = 1.
  points_dphi = 0.01
  call follow_field(points_move(1,:), points_dphi, dist)
  print *,'new point location',points_move(1,:)
  print *,'Lc',dist
  call diffuse_point(points_move(1,:), newp, dist, Te, D, 1)
  points_move(1,:) = newp
  print *,'after diffusion',points_move(1,:)
  
  

end program comprehensive_test
