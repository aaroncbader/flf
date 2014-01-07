program write_coils
  use coil_module
  implicit none

  integer :: i,j

  call allocate_main(6,14)
  call allocate_aux('aux_c.dat')
  call read_coils()
  
  open(40,file='coils.out',status='unknown')
  
  write(40,*) main_count + aux_count
  do i=1,main_count
     print *,i,main_points(i)
     write(40,*) main_points(i)
     do j=1,main_points(i)
        write(40,*) coil_main(i,j,1),coil_main(i,j,2),&
             coil_main(i,j,3)
     enddo
  enddo
  do i=1,aux_count
     print *,i,aux_points(i)
     write(40,*) aux_points(i)
     do j=1,aux_points(i)
        write(40,*) coil_aux(i,j,1),coil_aux(i,j,2),&
             coil_aux(i,j,3)
     enddo
  enddo

  call deallocate_coils()

end program write_coils
  
