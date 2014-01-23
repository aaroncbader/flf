! Read the input from the files
program read_input

  use points_module
  use coil_module
  use limiter_module
  use vessel_module
  use div_module
  implicit none

  character*72 :: input_file, line
  integer :: filenum, i
  real :: totcur, dummy

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

  ! Number of main coils
  call read_until_data(filenum, line)
  call string_to_int(line, num_main_coils)
  
  ! Current in main coils
  call read_until_data(filenum, line)
  call string_to_real(line, totcur)

  ! Allocate the main coils
  call allocate_main(14)

  ! Get the aux file
  call read_until_data(filenum, line)
  aux_file = trim(adjustl(line))

  ! Number of aux coils
  call read_until_data(filenum, line)
  call string_to_int(line, num_aux_coils)

  ! Allocate the aux values
  call allocate_aux(aux_file)
  
  ! taper values
  do i = 1,taper_size ! taper_size is redundnat, should be removed
     call read_until_data(filenum, line)
     call string_to_real(line, dummy)
     taper(i) = dummy
  end do

  call read_coil_files(totcur)
     
  ! VESSEL INFO
  call read_until_data(filenum, line)
  call string_to_int(line, num_vessels)
  do i=1,num_vessels
     call read_until_data(filenum, line)
     vessel_file = trim(adjustl(line))
  end do

  if (num_vessels.gt.0) then
     call allocate_vessel()
     call load_vessel()
  end if

  ! Limiter info
  call read_until_data(filenum, line)
  call string_to_int(line, num_limiters)
  do i=1,num_limiters
     call read_until_data(filenum, line)
     limiter_file = trim(adjustl(line))
  end do 

  ! Divertor info
  call read_until_data(filenum, line)
  call string_to_int(line, num_divertors)
  allocate(div_files(num_divertors))
  do i=1,num_limiters
     call read_until_data(filenum, line)
     div_files(i) = trim(adjustl(line))
  end do
  if (num_limiters.gt.0) then
     call read_until_data(filenum, line)
     axis_file = line
  end if

  ! No more to read

end program read_input
