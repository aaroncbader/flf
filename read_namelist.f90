subroutine read_namelist

  use points_module
  use coil_module
  use limiter_module
  use vessel_module
  use div_module
  use options_module
  use lcfs_module
  use mgrid_module

  implicit none

  integer :: numargs, iostat
  integer :: filenum, i
  character*72 :: input_file
  
  namelist / flf / points_file, points_number, points_dphi, n_iter, &
       num_periods, num_main_coils, is_mirrored, coil_file_input, &
       skip_value, aux_file, aux_percent, aux_flag, mgrid_file, &
       use_diffusion, diffusion_species, d_perp, temperature, boozer_step,&
       boozer_phi, axis_file, use_vessel, vessel_file, num_limiters,&
       lim_file, num_divertors, div_file, num_lcfs, lcfs_file

  filenum = 10
  input_file = 'flf.namelist'
  open(filenum, file=trim(input_file), status='old')
  read(filenum, nml=flf, iostat=iostat)
  
  write(*,*) points_file
  write(*,*) points_number
end subroutine read_namelist
