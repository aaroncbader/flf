program test_divread

  
  use div_module

  implicit none
  
  integer :: num_files, i,j,k
  character*144, dimension(:), allocatable :: filenames

  num_files = 1
  allocate(filenames(num_files))

  filenames(1) = 'DIV_island4x25_2'
  

  call alloc_div(filenames, num_files)

  do i = 1,num_files
     print *,div_tor_num(i), div_seg_num(i)
  end do

  call load_div(filenames)

  i = 1
  do j = 1,div_tor_num(i)
     print *, div_tor_vals(i,j)
     do k = 1,div_seg_num(i)
        print *, divertor(i,j,k,:)
     end do
  end do
  

  call deallocate_div()

end program test_divread
