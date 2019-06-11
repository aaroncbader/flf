subroutine read_namelist()

  use points_module
  use coil_module
  use limiter_module
  use vessel_module
  use div_module
  use options_module
  use lcfs_module
  use mgrid_module
  use gyro_module

  implicit none

  integer :: numargs, iostat
  integer :: filenum, i
  
  namelist / flf / points_file, points_number, points_dphi, n_iter, &
       log_freq, follow_type, field_type, &
       num_periods, num_main_coils, is_mirrored, coil_file_input, &
       skip_value, main_winding, results_file, &
       num_aux_coils, aux_file, mgrid_file, &
       use_diffusion, diffusion_species, d_perp, temperature, boozer_step,&
       varD, varDgamma, varDB0, & 
       boozer_phi, vessel_file, num_limiters, axis_file, &
       num_divertors, lcfs_file, general_option, output_coils, &
       use_gyro

  filenum = 10
  numargs = command_argument_count()
  if (numargs>=1) call get_command_argument(1, namelist_file)
  
  open(filenum, file=trim(namelist_file), status='old')
  read(filenum, nml=flf, iostat=iostat)
  
  !Initialize coils
  if (num_main_coils >= 1) then
     coil_type = 1
     allocate(main_files(num_main_coils))
     main_files(:) = ''
     !main count includes all coils not just for one period
     main_count = num_main_coils * num_periods * (is_mirrored + 1)
     allocate(main_current(main_count))
     main_current(:) = 0.0
     if (num_aux_coils >= 1) then
        allocate(aux_percent(num_aux_coils))
        aux_percent(:) = 0.0
     end if
     call coil_namelist(filenum)
  else !this is the mgrid setting
     coil_type = 2
     num_main_coils = 0
     num_aux_coils = 0
  end if

  !divertor initialization
  if (num_divertors >= 1) then
     allocate(div_files(num_divertors))
     if (axis_file == '') write (*,*) 'Error: Divertors need an axis file'
     div_files(:) = ''
     call div_namelist(filenum)     
  end if

  !limiter initialization
  if (num_limiters >= 1) then
     allocate(lim_files(num_limiters))
     lim_files(:) = ''
     call lim_namelist(filenum)     
  end if

  !use gyro
  if (use_gyro >= 1) then
     call gyro_namelist(filenum)
  end if
  
  close(filenum)
  
end subroutine read_namelist

!Read the info in the subsection of the namelist for coils
!This is the info about the coil file names, and the currents
!Also includes the information for the aux coils
!Needs to be in a separate place because we need to pre-initialize it
subroutine coil_namelist(filenum)

  use coil_module
  implicit none

  integer :: filenum, iostat, i

  namelist / coils / main_files, main_current, aux_percent, main_current_repeat
  read(filenum, nml=coils, iostat=iostat)
  

  do i = 1,num_main_coils
     main_files(i) = trim(main_files(i))
     if (main_files(i) == '') then 
        write(*,*) 'Warning: Not all files may have been loaded'
     end if
  end do

  if (main_current_repeat == 1) then
     do i = 2,num_main_coils
        main_current(i) = main_current(1)
     end do
  end if
  
end subroutine coil_namelist

subroutine div_namelist(filenum)
  use div_module
  implicit none

  integer :: filenum, iostat, i
  namelist / div / div_files
  read(filenum, nml=div, iostat=iostat)

  do i = 1,num_divertors
     div_files(i) = trim(div_files(i))
     if (div_files(i) == '') then
        write(*,*) 'Warning: not all divertor files may have been loaded'
     end if
  end do

end subroutine div_namelist

subroutine lim_namelist(filenum)
  use limiter_module
  implicit none

  integer :: filenum, iostat, i
  namelist / lim / lim_files, lim_halfwidth, lim_minstep
  read(filenum, nml=lim, iostat=iostat)

  do i = 1,num_limiters
     lim_files(i) = trim(lim_files(i))
     if (lim_files(i) == '') then
        write(*,*) 'Warning: not all limiter files may have been loaded'
     end if
  end do

end subroutine lim_namelist

subroutine gyro_namelist(filenum)
  use gyro_module
  implicit none

  integer :: filenum, iostat
  namelist / gyro / gyro_x, gyro_y, gyro_z, gyro_rad, gyro_I
  read(filenum, nml=gyro, iostat=iostat)

end subroutine gyro_namelist
