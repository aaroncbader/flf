program test_vessel

  implicit none
  
  real,allocatable,dimension(:,:,:) :: vessel
  integer,dimension(2) :: vessel_size
  character*144 :: vessel_file
  integer :: i,j,isin, inside_vessel
  real :: z

 
  !Block of code to get the vessel information
  vessel_file = 'vessel.txt'
  call allocate_vessel()
  call load_vessel()

  !open(40,file='vessel.out',status='unknown')
  !write(40,*) vessel_size(1:2)
  !do i=1,vessel_size(1)
  !   do j=1,vessel_size(2)
  !      write(40,*) vessel(i,j,1:3)
  !   enddo
  !enddo

  
  do i=0,10
     z = real(i)/100 + 0.2
     print *,'z',z
     isin = inside_vessel(1.1, z, 0.6, vessel, vessel_size)
     print *,isin
  enddo

  deallocate(vessel)
end program test_vessel
