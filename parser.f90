
! Function to read a comment line
subroutine read_string(filenum, line)

  implicit none
  character(len=*) :: line
  integer :: filenum
  !print *,line

 
  read(filenum,'(A)') line

end subroutine read_string

! Function to determine a comment line
integer function is_comment(line)

  implicit none
  character(len=*) :: line
  
  if (line(1).eq.'!') then
     is_comment = 1
  else
     is_comment = 0
  end if
 
  return
end function is_comment


! Read files until you get data
subroutine read_until_data(filenum, line)

  implicit none
  integer :: filenum, is_comment
  character(len=*) :: line
  character :: beginning

  line = ''
  do 
     call read_string(filenum, line)
     beginning = line(1:1)
     if (beginning.ne.'!') then 
        exit
     end if
  end do

end subroutine read_until_data

! convert a string into an array of reals
subroutine string_to_reals(line, array, array_size)

  implicit none
  integer :: array_size
  character(72) :: line
  double precision, dimension(array_size) :: array

  read (line,*) array(1:array_size)

end subroutine string_to_reals

! convert a string into a single real
subroutine string_to_real(line, answer)

  implicit none
  double precision :: answer
  character(len=*) :: line


  read (line,*) answer

end subroutine string_to_real

!convert a string into an array of ints
subroutine string_to_ints(line, array, array_size)
  implicit none
  integer :: array_size
  character(len=*) :: line
  integer, dimension(array_size) :: array

  read (line,*) array(1:array_size)

end subroutine string_to_ints

!convert a string into an array of ints
subroutine string_to_int(line, answer)
  implicit none
  integer :: answer
  character(len=*) :: line

  read (line,*) answer

end subroutine string_to_int


  
