subroutine allocate_main(skip_value)

  use coil_module
  implicit none
  integer :: mult_factor

  integer :: i
  integer :: filenum, temp_size, skip_value
  character*15 :: filename, format_string

  mult_factor = coil_sections * (is_mirrored + 1)
  ! set values for the coil module
  main_count = num_main_coils * mult_factor
  skip = skip_value
  main_size = 0

  ! This block is used twice, we can probably extract it out
  ! generalize the coil loading for multiple coils
  do i=1,num_main_coils
     filenum = 30 + i

     ! if more than 10 coils we need two characters for the string
     if (i < 10) then
        format_string = '(A1,I1,A4)'
     else
        format_string = '(A1,I2,A4)'
     endif
     write (filename, format_string),'c',i,'.dat'
     open(filenum, file=filename, status='old', form='formatted')

     read(filenum,*) temp_size
     !set the temp_size if necessary
     if (temp_size / skip > main_size) then
        main_size = temp_size / skip
     endif

     close(filenum)

  enddo

  !Add an extra null points to be safe 
  !(we have the total value saved elsewhere anyway)
  main_size = main_size + 1

  !Allocate the array to be the size of the largest one
  allocate(coil_main(main_count, main_size, 3))
  allocate(main_current(main_count))
  allocate(main_points(main_count))

end subroutine allocate_main

subroutine allocate_aux()
  use coil_module
  implicit none

  integer :: mult_factor
  integer :: i,j,k

  integer :: temp_size, dummy, filenum
  real :: x,y,z !dummy variables

  mult_factor = coil_sections * (is_mirrored + 1)

  filenum = 41

  open(filenum, file=trim(aux_file), status='old', form='formatted')
  read(filenum,*) dummy
  
  taper_size = num_aux_coils
  aux_count = num_aux_coils * mult_factor
  aux_size = 0
  do i=1,num_aux_coils
     read(filenum,*) temp_size
     do j=1,temp_size
        read(filenum,*) x,y,z
     enddo
     ! save the largest value in aux_size
     if (temp_size > aux_size) then
        aux_size = temp_size
     end if
  end do

  ! Give a point of slop (for wraparound if desired)
  aux_size = aux_size+1

  ! do the allocations
  allocate(coil_aux(aux_count, aux_size, 3))
  allocate(aux_current(aux_count))
  allocate(aux_points(aux_count))
  allocate(taper(taper_size))

  close(filenum)
end subroutine allocate_aux

subroutine deallocate_coils()
  use coil_module
  deallocate(coil_main)
  deallocate(coil_aux)
  deallocate(main_current)
  deallocate(aux_current)
  deallocate(main_points)
  deallocate(aux_points)
  deallocate(taper)
end subroutine deallocate_coils
  

subroutine read_coil_files(current)


  use coil_module
  implicit none

  integer, parameter :: mult_factor=8

  integer :: i,j,k
  integer :: piece, filenum, total_points, dummy
  real :: current,x,y,z
  character*15 :: filename, format_string
  
  


  

  ! generalize the coil loading for multiple coils
  do i=1,num_main_coils
     filenum = 30 + i

     filename = trim(main_files(i))
     
     open(filenum, file=filename, status='old', form='formatted')
  enddo

  

  ! iterate over coil files
  do i=1,num_main_coils
     piece = 0 !reset piece value

     filenum = i + 30

     read(filenum,*) total_points

     ! Konstantin skips the first one, maybe we should too?
     read(filenum,*) x,y,z

     ! read all individual coil points into the first six places
     do j = 1, total_points, skip

        !increment the piece value
        piece = piece + 1

        read(filenum,*) x,y,z
        coil_main(i,piece,1) = x
        coil_main(i,piece,2) = y
        coil_main(i,piece,3) = z

        ! Get out if we we're at the end
        if (j + skip .gt. total_points) then 
           exit !this is fortran's "break"
        endif

        ! dump the values we're not using into dummy vars
        do k=1,skip-1
           read(filenum,*) x,y,z

        enddo !end of dumping loop
     enddo ! end of individual file loop
     ! Load number of segments
     main_points(i) = piece

     close(filenum)
  enddo !end of all files

  ! All aux coils are in one file, no need to skip
  open(67,file=aux_file,status='old',form='formatted')
  read(67,*) dummy
  do i=1,num_aux_coils
     read(67,*) aux_points(i)
     do j=1,aux_points(i)
        read(67,*) x,y,z
        coil_aux(i,j,1) = x
        coil_aux(i,j,2) = y
        coil_aux(i,j,3) = z
     enddo
  enddo
  close(67)

  call move_coils(coil_main, main_points, &
       num_main_coils, main_size)
  call move_coils(coil_aux, aux_points, num_aux_coils, &
       aux_size)
  !Input the currents into the main coils
  do i=1,num_main_coils * mult_factor
     ! There are 14 windings in the main coils, this is independent of the
     ! skip value
     main_current = current/14 
  enddo

  !Currents for auxiliary coils
  do i=1,taper_size
     do j=0,mult_factor-1
        aux_current(i + (j*taper_size)) = current*taper(i)
     enddo
  enddo

  return
end subroutine read_coil_files


! It's harder to generalize this for devices that do not have 
! 4 part symmetry, so i'm not going to bother
! It would also require significantly more calculation
subroutine move_coils(coils, coil_counts, coil_number, piece_count)

  implicit none

  integer, parameter :: mult_factor = 8

  integer :: i,j,revj
  integer :: piece_count, coil_number, coil_index
  real :: coils(coil_number * mult_factor, piece_count, 3)
  integer :: coil_counts(coil_number * mult_factor)


  do i=1,coil_number !loop for the coils
     !update the coil counts
     do coil_index = i,coil_number * mult_factor, coil_number
        coil_counts(coil_index) = coil_counts(i)
     enddo

     do j=1,coil_counts(i)!loop over the individual coils
        revj = coil_counts(i) - j + 1
        !period A', swap x->y, y->x, z->-z
        coil_index = i+coil_number !indices 7-12
        coils(coil_index,j,1) = coils(i,revj,2)
        coils(coil_index,j,2) = coils(i,revj,1)
        coils(coil_index,j,3) = -coils(i,revj,3)

        !period B,  x->y, y->-x, z->z
        coil_index = i+(coil_number*2) !indices 13-18
        coils(coil_index,j,1) = coils(i,j,2)
        coils(coil_index,j,2) = -coils(i,j,1)
        coils(coil_index,j,3) = coils(i,j,3)

        !period B', x->-x, y->y, z->-z
        coil_index = i+(coil_number*3) !indices 19-24
        coils(coil_index,j,1) = -coils(i,revj,1)
        coils(coil_index,j,2) = coils(i,revj,2)
        coils(coil_index,j,3) = -coils(i,revj,3)

        !period C, x->-x, y->-y, z->z
        coil_index = i+(coil_number*4) !indices 25-30
        coils(coil_index,j,1) = -coils(i,j,1)
        coils(coil_index,j,2) = -coils(i,j,2)
        coils(coil_index,j,3) = coils(i,j,3)

        !period C', x->-y, y->-x, z->-z
        coil_index = i+(coil_number*5) !indices 31-36
        coils(coil_index,j,1) = -coils(i,revj,2)
        coils(coil_index,j,2) = -coils(i,revj,1)
        coils(coil_index,j,3) = -coils(i,revj,3)

        !period D, x->-y, y->x, z->z
        coil_index = i+(coil_number*6) !indices 37-43
        coils(coil_index,j,1) = -coils(i,j,2)
        coils(coil_index,j,2) = coils(i,j,1)
        coils(coil_index,j,3) = coils(i,j,3)

        !period D, x->x, y->-y, z->-z
        coil_index = i+(coil_number*7) !indices 44-48
        coils(coil_index,j,1) = coils(i,revj,1)
        coils(coil_index,j,2) = -coils(i,revj,2)
        coils(coil_index,j,3) = -coils(i,revj,3)

     enddo
  enddo

  return
end subroutine move_coils

