subroutine get_vessel_dimensions(vessel_file, vessel_size)
! hello world
  implicit none
  character*144 :: vessel_file
  integer,dimension(2) :: vessel_size
  integer :: filenum = 21

  open(filenum, file=vessel_file, status='old', form = 'formatted')
  read(filenum,*) vessel_size(1:2)
  close(filenum)

end subroutine get_vessel_dimensions

subroutine load_vessel(vessel_file, vessel, vessel_size)
  implicit none

  integer, dimension(2) :: vessel_size
  character*144 :: vessel_file
  character*72 :: dummy
  integer :: vessel_tor, vessel_pol, i,j
  real,dimension(vessel_size(1),vessel_size(2),3) :: vessel
  real :: x,y,z
  integer :: filenum = 21

  open(filenum,file=vessel_file,status='old',form='formatted')

  
  ! the first two value should give the number of toroidal and poloidal
  ! points respectively.
  read(filenum,*) dummy
  print *,dummy

  do i=1,vessel_size(1)
     do j=1,vessel_size(2)
!        print *,i,j
        read(filenum,*) vessel(i,j,1:3)
     enddo
  enddo
  close(filenum)

end subroutine load_vessel
