program read_sample_input

  implicit none
  
  character*72 :: input_file, line
  integer :: filenum, fcount,ncount,i
  character*72 :: file1
  character*72, dimension(:), allocatable :: filearray
  double precision, dimension(:,:), allocatable :: numbers
  
  
  filenum = 10

  input_file = 'sample_input.txt'

  open(filenum, file=trim(input_file), status='old', form='formatted')

  ! start the reading
  call read_until_data(filenum, line)

  file1 = line
 
  print *,file1

  call read_until_data(filenum, line)
  call string_to_int(line, fcount)

  allocate(filearray(fcount))
  do i =1,fcount
     call read_string(filenum, line)
     filearray(i) = line
  end do

  do i = 1,fcount
     print *,filearray(i)
  end do

  call read_until_data(filenum, line)
  call string_to_int(line, ncount)
  allocate(numbers(ncount,2))
  do i=1,ncount
     call read_string(filenum, line)
     call string_to_reals(line,numbers(i,:),2)
  end do
  
  do i=1,ncount
     print *,numbers(i,:)
  end do
  
  print *,int(numbers(4,2))

end program read_sample_input
  
  
