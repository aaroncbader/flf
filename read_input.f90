! Read the input from the files
subroutine read_input

  use points_module
  use coil_module
  use limiter_module
  use vessel_module
  use div_module
  use options_module
  use lcfs_module
  implicit none

  character*72 :: input_file, line
  integer :: filenum, i
  real :: totcur, dummy
  integer, allocatable, dimension(:) :: dummy_int
  real, allocatable, dimension(:) :: dummy_arr

  filenum = 10
  input_file = 'flf.input'
  open(filenum, file=trim(input_file), status='old', form='formatted')

  ! Get the name for the points file
  call read_until_data(filenum, line)
  points_file = trim(adjustl(line))

  ! Get the number of points
  call read_until_data(filenum, line)
  call string_to_int(line, points_number)

  ! Get the shift amount
  call read_until_data(filenum, line)
  call string_to_real(line, points_dphi)
  
  ! Get the number of iterations
  call read_until_data(filenum, line)
  call string_to_int(line, n_iter)

  ! Results file
  call read_until_data(filenum, line)
  results_file = line

  !----------------------------------
  ! Coil information
  ! symmetry information
  call read_until_data(filenum, line)
  allocate(dummy_int(2))
  call string_to_ints(line, dummy_int, 2)
  coil_sections = dummy_int(1)
  is_mirrored = dummy_int(2)
  deallocate(dummy_int)

  ! Number of main coils
  call read_until_data(filenum, line)
  call string_to_int(line, num_main_coils)

  !allocate files for main coils
  allocate(main_files(num_main_coils))

  ! Names of main coils
  do i=1,num_main_coils
     call read_until_data(filenum, line)
     main_files(i) = line
  end do

  call read_until_data(filenum, line)
  call string_to_int(line, skip_value)

  ! Allocate the main coils
  call allocate_main()

  ! Current in main coils
  do i = 1,num_main_coils
     call read_until_data(filenum, line)
     call string_to_real(line, main_current(i))
  end do


  ! Get the aux file
  call read_until_data(filenum, line)
  aux_file = trim(adjustl(line))

  ! Number of aux coils and windings
  call read_until_data(filenum, line)
  allocate(dummy_int(2))
  call string_to_ints(line, dummy_int, 2)
  num_aux_coils = dummy_int(1)
  main_winding = dummy_int(2)
  deallocate(dummy_int)

  ! Allocate the aux values
  call allocate_aux(aux_file)
  
  ! taper values
  do i = 1,num_aux_coils ! taper_size is redundant, should be removed
     call read_until_data(filenum, line)
     call string_to_real(line, dummy)
     taper(i) = dummy
  end do

  call read_coil_files()

  ! DIFFUSION INFO ------------------------
  ! is diffusion on
  call read_until_data(filenum, line)
  call string_to_int(line, use_diffusion)

  ! what species to use
  call read_until_data(filenum, line)
  call string_to_int(line, diffusion_species)

  ! temperature and D
  call read_until_data(filenum, line)
  allocate(dummy_arr(2))
  call string_to_reals(line, dummy_arr, 2)
  diffusion_D = dummy_arr(1)
  temperature = dummy_arr(2)
  deallocate(dummy_arr)
     
  ! VESSEL INFO --------------------------
  call read_until_data(filenum, line)
  call string_to_int(line, num_vessels)
  do i=1,num_vessels
     call read_until_data(filenum, line)
     vessel_file = trim(adjustl(line))
  end do
  
  allocate(points_hit_vessel(points_number))

  ! Right now, can only handle one vessel
  if (num_vessels.gt.0) then
     call allocate_vessel()
     call load_vessel()
     points_hit_vessel(:) = 0
  end if
  

  ! Limiter info
  ! changed to allow more than one limiter
  call read_until_data(filenum, line)
  call string_to_int(line, num_limiters)
  allocate(lim_files(num_limiters))
  do i=1,num_limiters
     call read_until_data(filenum, line)
     lim_files(i) = trim(adjustl(line))
  end do

  allocate(points_hit_limiter(points_number))
  
  if (num_limiters.gt.0) then
     call allocate_limiter()
     call load_limiter()
     points_hit_limiter(:) = 0
  end if

  

  ! Divertor info
  call read_until_data(filenum, line)
  call string_to_int(line, num_divertors)
  allocate(div_files(num_divertors))
  do i=1,num_divertors
     call read_until_data(filenum, line)
     div_files(i) = trim(adjustl(line))
  end do
  if (num_divertors.gt.0) then
     call read_until_data(filenum, line)
     axis_file = line
  end if
  allocate(points_hit_divertor(points_number))

  if (num_divertors.gt.0) then
     call alloc_div()
     call load_div()
     call load_axis()
     points_hit_divertor(:) = 0
  end if

  call read_until_data(filenum, line)
  call string_to_int(line, num_lcfs)
  if (num_lcfs.gt.0) then
     call read_until_data(filenum, line)
     lcfs_file = trim(adjustl(line))
     call allocate_lcfs()
     !	print *, 'number of LCFS:', num_lcfs
     call load_lcfs()
     !	print *, 'number of LCFS:', num_lcfs
  end if


  ! No more to read

end subroutine read_input
