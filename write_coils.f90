subroutine write_coils
  use coil_module
  implicit none

  integer :: i,j

  

  open(40,file='coils.out',status='unknown')
  
  write(40,*) main_count + aux_count
  do i=1,main_count
     print *,i,main_points(i)
     write(40,*) main_points(i), main_current(i)
     do j=1,main_points(i)
        write(40,*) coil_main(i,j,1),coil_main(i,j,2),&
             coil_main(i,j,3)
     enddo
  enddo
  do i=1,aux_count
     print *,i,aux_points(i)
     write(40,*) aux_points(i), aux_current(i)
     do j=1,aux_points(i)
        write(40,*) coil_aux(i,j,1),coil_aux(i,j,2),&
             coil_aux(i,j,3)
     enddo
  enddo

  call deallocate_coils()

end subroutine write_coils
  
