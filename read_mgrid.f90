subroutine allocate_mgrid_ascii

   use mgrid_module
   implicit none

   integer :: filenum
   character*72 :: filename

   filenum = 50
   filename = trim(mgrid_file)

   open(filenum, file=filename, status='old')

   read(filenum, *) mgrid_nr, mgrid_nz, mgrid_nphi
   close(filenum)
   allocate(mgrid_r(mgrid_nr))
   allocate(mgrid_z(mgrid_nz))
   allocate(mgrid_phi(mgrid_nphi))

   allocate(mgrid_br(mgrid_nphi, mgrid_nz, mgrid_nr))
   allocate(mgrid_bz(mgrid_nphi, mgrid_nz, mgrid_nr))
   allocate(mgrid_bphi(mgrid_nphi, mgrid_nz, mgrid_nr))


end subroutine allocate_mgrid_ascii

subroutine load_mgrid_ascii
    use mgrid_module
    implicit none

    integer :: filenum, i, j
    character*72 :: filename
    
    filename = trim(mgrid_file)
    open(filenum, file=filename, status='old')
    read(filenum, *) mgrid_nr, mgrid_nz, mgrid_nphi

    !read the axis values
    read(filenum,*) mgrid_r
    read(filenum,*) mgrid_z
    read(filenum,*) mgrid_phi

    mgrid_rmin = mgrid_r(1)
    mgrid_rmax = mgrid_r(mgrid_nr)
    mgrid_zmin = mgrid_z(1)
    mgrid_zmax = mgrid_z(mgrid_nz)
    mgrid_phimin = mgrid_phi(1)
    mgrid_phimax = mgrid_phi(mgrid_nphi)

    !now read the data
    do i=1,mgrid_nphi
      do j=1,mgrid_nz
        read(filenum,*) mgrid_br(i,j,:)
      end do
    end do
    do i=1,mgrid_nphi
      do j=1,mgrid_nz
        read(filenum,*) mgrid_bz(i,j,:)
      end do
    end do
    do i=1,mgrid_nphi
      do j=1,mgrid_nz
        read(filenum,*) mgrid_bphi(i,j,:)
      end do
    end do
    close(filenum)


end subroutine load_mgrid_ascii


! There might be some nice canned routines to shuffle around
! indicies, but I couldn't find them, so we'll do this the
! manual way. Speed shouldn't be an issue since this only
! needs to be done once on file load
subroutine mgrid_add_bfield(mgrid_btemp, mgrid_bload, current)
  use mgrid_module, only: mgrid_nz, mgrid_nphi, mgrid_nr
  implicit none

  real, dimension(mgrid_nphi, mgrid_nz, mgrid_nr) :: mgrid_bload
  real, dimension(mgrid_nr, mgrid_nz, mgrid_nphi) :: mgrid_btemp
  real :: current
  integer :: np, nz, nr
  
  do np = 1,mgrid_nphi
     do nz = 1, mgrid_nz
        do nr = 1, mgrid_nr
           mgrid_bload(np, nz, nr) = &
                mgrid_bload(np, nz, nr) + current * mgrid_btemp(nr, nz, np)
        end do
     end do
  end do
end subroutine mgrid_add_bfield  

subroutine load_mgrid_netcdf
  use mgrid_module
  use options_module, only: namelist_file
  use coil_module, only: num_periods
  implicit none
  include 'netcdf.inc'

  integer :: status, ncid, varid, filenum, iostat
  integer :: mgrid_numcoils
  integer :: i, nfp
  real, dimension(:,:,:), allocatable :: mgrid_btemp
  real :: dr, dz, dphi
  character*200 :: dummy, varname
  character(8) :: fmt
  character(3) :: var_index 

  namelist / coils / mgrid_currents

  status = nf_open(mgrid_file, nf_write, ncid)
  
  status = nf_inq_dimid(ncid, 'phi', varid) 
  status = nf_inq_dim(ncid, varid, dummy, mgrid_nphi)

  status = nf_inq_dimid(ncid, 'rad', varid) 
  status = nf_inq_dim(ncid, varid, dummy, mgrid_nr)

  status = nf_inq_dimid(ncid, 'zee', varid) 
  status = nf_inq_dim(ncid, varid, dummy, mgrid_nz) 
  
  status = nf_inq_dimid(ncid, 'external_coils', varid) 
  status = nf_inq_dim(ncid, varid, dummy, mgrid_numcoils)

  !allocate the currents
  allocate(mgrid_currents(mgrid_numcoils))
  !set all currents to zero
  mgrid_currents = 0
  !read the coil namelist
  filenum = 10
  open(filenum, file=trim(namelist_file), status='old')
  read(filenum, nml=coils, iostat=iostat)
  close(filenum)
  
  !write(*,*) 'ext coils', mgrid_numcoils
  !write(*,*) 'coil currents', mgrid_currents
  !write(*,*) 'mgrid nr, nz, nphi', mgrid_nr, mgrid_nz, mgrid_nphi

  allocate(mgrid_br(mgrid_nphi, mgrid_nz, mgrid_nr))
  mgrid_br = 0
  allocate(mgrid_bz(mgrid_nphi, mgrid_nz, mgrid_nr))
  mgrid_bz = 0
  allocate(mgrid_bphi(mgrid_nphi, mgrid_nz, mgrid_nr))
  mgrid_bphi = 0
  
  !unfortunately the mgrid reads in backwards order
  allocate(mgrid_btemp(mgrid_nr, mgrid_nz, mgrid_nphi)) 
  
  !start loading data
  fmt = '(I3.3)'
  do i = 1,mgrid_numcoils
     !ignore zero current coils
     if (abs(mgrid_currents(i)) <= 0.00000001) cycle

     !need to create the variable name 
     write (var_index, fmt) i !convert i to a size 3 string
     varname = 'bp_'//trim(var_index) !assemble the bphi variable
     
     status = nf_inq_varid(ncid, varname, varid)  
     status = nf_get_var_double(ncid, varid, mgrid_btemp)
     !convert the data
     call mgrid_add_bfield(mgrid_btemp, mgrid_bphi, mgrid_currents(i))

     !now do the same for br
     varname = 'br_'//trim(var_index) !assemble the bphi variable
     
     status = nf_inq_varid(ncid, varname, varid)  
     status = nf_get_var_double(ncid, varid, mgrid_btemp)
     !convert the data
     call mgrid_add_bfield(mgrid_btemp, mgrid_br, mgrid_currents(i))

     !and finally bz
     varname = 'bz_'//trim(var_index) !assemble the bphi variable
     
     status = nf_inq_varid(ncid, varname, varid)  
     status = nf_get_var_double(ncid, varid, mgrid_btemp)
     !convert the data
     call mgrid_add_bfield(mgrid_btemp, mgrid_bz, mgrid_currents(i))    
  end do

  !last step is to assemble the r, z, and phi arrays
  allocate(mgrid_r(mgrid_nr))
  allocate(mgrid_z(mgrid_nz))
  allocate(mgrid_phi(mgrid_nphi))

  !grab the min and max r values
  status = nf_inq_varid(ncid, 'rmin', varid)
  status = nf_get_var_double(ncid, varid, mgrid_rmin)
  status = nf_inq_varid(ncid, 'rmax', varid)
  status = nf_get_var_double(ncid, varid, mgrid_rmax)
  !assemble the r array
  dr = (mgrid_rmax - mgrid_rmin)/(mgrid_nr-1)
  mgrid_r(1) = mgrid_rmin
  do i = 1,mgrid_nr-2
     mgrid_r(i+1) = mgrid_r(i) + dr
  end do
  mgrid_r(mgrid_nr) = mgrid_rmax !set the max to avoid rounding errors

  !do the same for z
  status = nf_inq_varid(ncid, 'zmin', varid)
  status = nf_get_var_double(ncid, varid, mgrid_zmin)
  status = nf_inq_varid(ncid, 'zmax', varid)
  status = nf_get_var_double(ncid, varid, mgrid_zmax)
  !assemble the r array
  dz = (mgrid_zmax - mgrid_zmin)/(mgrid_nz-1)
  mgrid_z(1) = mgrid_zmin
  do i = 1,mgrid_nz-2
     mgrid_z(i+1) = mgrid_z(i) + dz
  end do
  mgrid_z(mgrid_nz) = mgrid_zmax
  

  !run a check for the mgrid number of periods
  status = nf_inq_varid(ncid, 'nfp', varid)
  status = nf_get_var_int(ncid, varid, nfp)
  if (nfp .ne. num_periods) then
     write(*,*) 'WARNING: input number of periods ', num_periods, 'does not match mgrid: ',nfp
     write(*,*) 'Using mgrid number of periods'
     num_periods = nfp
  end if
  
  
  !and for phi
  mgrid_phimin = 0.0
  mgrid_phimax = 3.14159265358979323*2/num_periods
  dphi = (mgrid_phimax - mgrid_phimin)/(mgrid_nphi - 1)
  mgrid_phi(1) = mgrid_phimin
  do i = 1,mgrid_nphi-2
     mgrid_phi(i+1) = mgrid_phi(i) + dphi
  end do
  mgrid_phi(mgrid_nphi) = mgrid_phimax
  
end subroutine load_mgrid_netcdf


subroutine deallocate_mgrid
  use mgrid_module

  if (allocated(mgrid_r)) deallocate(mgrid_r)
  if (allocated(mgrid_z)) deallocate(mgrid_z)
  if (allocated(mgrid_phi)) deallocate(mgrid_phi)
  if (allocated(mgrid_br)) deallocate(mgrid_br)
  if (allocated(mgrid_bz)) deallocate(mgrid_bz)
  if (allocated(mgrid_bphi)) deallocate(mgrid_bphi)

end subroutine deallocate_mgrid
