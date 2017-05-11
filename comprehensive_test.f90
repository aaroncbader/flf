subroutine comprehensive_test
  use points_module
  use coil_module
  use div_module
  use options_module
  use vessel_module

  integer :: i,j, inside_div, isok, answer, inside_vessel, istate
  double precision :: dist, D, Te, z
  double precision, dimension(3) :: newp

  !Run some tests to make sure everything is correct
  !with loading and other stuff
  
  !Make sure the loaded file is the sample input file
  call get_points()
  !need to reload the proper coil files, do it manually
  deallocate(coil_main)
  if (allocated(coil_aux)) deallocate(coil_aux)
  if (allocated(aux_percent)) deallocate(aux_percent)
  deallocate(main_points)
  if (allocated(aux_points)) deallocate(aux_points)
  deallocate(main_files)
  deallocate(main_current)

  allocate(main_files(6))
  allocate(main_current(48))
  allocate(aux_percent(6))
  main_files(1) = 'c1.dat'
  main_files(2) = 'c2.dat'
  main_files(3) = 'c3.dat'
  main_files(4) = 'c4.dat'
  main_files(5) = 'c5.dat'
  main_files(6) = 'c6.dat'
  skip_value = 14
  num_main_coils = 6
  aux_file = 'aux_icdiv.dat'
  num_aux_coils = 6
  num_periods = 4
  is_mirrored = 1
  num_vessels=0
  
  main_current(:) = -150105.0
  call allocate_main()
  
  call allocate_aux()
  call read_coil_files()
  taper(:) = 0.0
  aux_current(:) = 0.0
  
  !Turn off the limiter for now
  num_limiters = 0
  points_dphi = 1.0
  current_point = 1

  current_step = 1
  
  points_start(1,1) = 1.45
  points_start(1,2) = 0.
  points_start(1,3) = 0.

  points_move(1,:) = points_start(1,:)
  !write (*,'(3(F10.7,2X))'),points_move(1,:)
  call follow_field(points_move(1,:), points_dphi, dist, istate)
  !write (*,'(3(F10.7,2X))'),points_move(1,:)
  if ((points_move(1,1).gt.1.09181).and.(points_move(1,1).lt.(1.09183)).and.&
      (points_move(1,2).gt.(-0.10317)).and.(points_move(1,2).lt.(-0.10316)))&
      then
     write(*,*) 'test with no auxiliary current PASS'
     write(*,*) points_move(1,:)
  else
     print *,'test with no auxiliary current FAIL'
     write(*,*) points_move(1,:)
  end if
  
  !add some aux currents to the mix, assumes we loaded 6 coils
  do i=1,6
     taper(i) = (0.02*i)
     do j=0,7
        aux_current(i + (j*6)) = main_current(1)* 14*0.02*i
     end do
  end do

  points_move(1,:) = points_start(1,:)
  call follow_field(points_move(1,:), points_dphi, dist, istate)
  !print *,points_start
  !print *,points_move
  if ((points_move(1,1).gt.1.09810).and.(points_move(1,1).lt.(1.09812)).and.&
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

  vessel_file = 'vessel.txt'
  if (allocated(vessel)) deallocate(vessel)
  call allocate_vessel()
  call load_vessel()
  num_vessels = 1
  isok = 1
  write (*,*) 'vessel size', vessel_size
  do i=0,10
     z = dble(i)/100 + 0.2
     !print *,'z',z
     answer = inside_vessel(1.3, z, 0.39269, vessel, vessel_size)
     !isin = inside_vessel(1.3, z, 1.0, vessel, vessel_size)
     if ((z.gt.0.235).and.(answer.eq.1)) isok = 0
     if ((z.lt.0.235).and.(answer.eq.0)) isok = 0
  enddo
  if (isok.eq.0) then
     print *,'Vessel test 1 FAIL'
  else
     print *,'Vessel test 1 PASS'
  end if  

  isok = 1
  do i=0,10
     z = dble(i)/100 + 0.2
     z = z*(-1)
     !print *,'z',z
     answer = inside_vessel(1.3, z, 1.17809, vessel, vessel_size)
     !isin = inside_vessel(1.3, z, 1.0, vessel, vessel_size)
     if ((z.lt.-0.235).and.(answer.eq.1)) isok = 0
     if ((z.gt.-0.235).and.(answer.eq.0)) isok = 0
  enddo
  if (isok.eq.0) then
     print *,'Vessel test 2 FAIL'
  else
     print *,'Vessel test 2 PASS'
  end if  



  !! tests for diffusion
  points_start(1,1) = 1.45
  points_start(1,2) = 0.
  points_start(1,3) = 0.
  call init_random_seed()
  Te = 50
  D = 1
  print *,Te, D
  print *,use_diffusion, diffusion_species
  points_dphi = 0.01
  call follow_field(points_move(1,:), points_dphi, dist, istate)
  print *,'new point location',points_move(1,:)
  print *,'Lc',dist
  call diffuse_point(points_move(1,:), newp, dist, Te, D, 1)
  print *,'after normal diffusion',newp

  !!test for boozer diffusion
  step = 0.01
  call diffuse_boozer(points_move(1,:), newp, step)
  print *,'after boozer diffusion',newp
  
  

end subroutine comprehensive_test
