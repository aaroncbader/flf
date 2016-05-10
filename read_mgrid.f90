subroutine allocate_mgrid

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


end subroutine allocate_mgrid

subroutine load_mgrid
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


end subroutine load_mgrid

subroutine deallocate_mgrid
  use mgrid_module

  if (allocated(mgrid_r)) deallocate(mgrid_r)
  if (allocated(mgrid_z)) deallocate(mgrid_z)
  if (allocated(mgrid_phi)) deallocate(mgrid_phi)
  if (allocated(mgrid_br)) deallocate(mgrid_br)
  if (allocated(mgrid_bz)) deallocate(mgrid_bz)
  if (allocated(mgrid_bphi)) deallocate(mgrid_bphi)

end subroutine deallocate_mgrid
