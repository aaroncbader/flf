program write_coils
  use coil_module
  implicit none

  integer :: i,j

  call read_coils()
  
  open(40,file='coils.out',status='unknown')
  
  write(40,*) main_count + aux_count
  do i=1,main_count
     print *,i,coil_set%main_points(i)
     write(40,*) coil_set%main_points(i)
     do j=1,coil_set%main_points(i)
        write(40,*) coil_set%main(i,j,1),coil_set%main(i,j,2),&
             coil_set%main(i,j,3)
     enddo
  enddo
  do i=1,aux_count
     print *,i,coil_set%aux_points(i)
     write(40,*) coil_set%aux_points(i)
     do j=1,coil_set%aux_points(i)
        write(40,*) coil_set%aux(i,j,1),coil_set%aux(i,j,2),&
             coil_set%aux(i,j,3)
     enddo
  enddo

end program write_coils
  
