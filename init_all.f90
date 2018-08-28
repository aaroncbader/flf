subroutine init_all

  use div_module
  use vessel_module
  use coil_module
  use points_module
  use limiter_module
  use lcfs_module
  use options_module
  implicit none


  ! initialize coils
  if ((num_main_coils >= 1) .and. (trim(field_type) == 'coils')) then
     call allocate_main()
     call allocate_aux()
     call read_coil_files()
     if (output_coils == 1) call write_coils()
  elseif (trim(field_type) == 'ascii') then
     call allocate_mgrid_ascii()
     call load_mgrid_ascii()
  elseif (trim(field_type) == 'netcdf') then
     call allocate_mgrid_netcdf()
     call load_mgrid_netcdf()
  else
     write (*,*) 'unknown coil type, aborting'
     return
  end if

  ! initialize divertors
  if (num_divertors >= 1) then
     call alloc_div()
     call load_div()
     call load_axis()
     allocate(points_hit_divertor(points_number))
     points_hit_divertor(:) = 0
  end if
  
  ! initialize limiters
  if (num_limiters >= 1) then
     call allocate_limiter()
     call load_limiter()
     allocate(points_hit_limiter(points_number))
     points_hit_limiter(:) = 0
  end if

  !initialize vessel
  if (vessel_file /= '') then
     call allocate_vessel()
     call load_vessel()
     allocate(points_hit_vessel(points_number))
     points_hit_vessel(:) = 0
     num_vessels = 1
  end if

  !initialize lcfs
  if (lcfs_file /= '') then
     call allocate_lcfs()
     call load_lcfs()
     if (axis_file=='') then
        write(*,*) 'Error: LCFS usage requires an access file'
     end if
     call load_axis()
  end if
  
  !also need to load axis for boozer diffusion
  if (use_diffusion == 2) then
     call load_axis()
  end if

     

end subroutine init_all
