program test_divread

  
  use div_module

  implicit none
  
  integer :: num_files, i,j,k, inside_div, dummy
  character*144, dimension(:), allocatable :: filenames
  character*144 :: axis_file

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
     !print *, div_tor_vals(i,j)
     do k = 1,div_seg_num(i)
        !print *, divertor(i,j,k,:)
     end do
  end do

  axis_file = 'mag_axis.dat'

  call load_axis(axis_file)

  do i=1,axis_points
     !print *,mag_axis(i,:)
  end do

  dummy = inside_div(1., 1., 0.0085)
  

  call deallocate_div_and_axis()

end program test_divread
