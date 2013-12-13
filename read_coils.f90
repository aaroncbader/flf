subroutine read_coils(coil_set)
  
  use coil_module
  implicit none

  type(coils) :: coil_set

  integer :: skip,i
  real :: current
  real, dimension(taper_size) :: taper


  do i=1,taper_size
     taper(i) = i*0.1
  enddo

  current = -150108.
  skip = 14

  call read_coil_files(taper, coil_set, current, skip)

!  do i=1,48
!     print *,coil_set%aux_current(i)
!  enddo

end subroutine read_coils


subroutine read_coil_files(taper, coil_set, current, skip)


  use coil_module
  implicit none

  integer, parameter :: mult_factor=8

  integer :: i,j,k
  integer :: skip, piece, filenum, total_points
  integer :: nmain, naux !number of main and aux coils
  real :: taper(*), current,x,y,z
  type(coils) coil_set

  open(31,file='c1.dat',status='old',form='formatted')
  open(32,file='c2.dat',status='old',form='formatted')
  open(33,file='c3.dat',status='old',form='formatted')
  open(34,file='c4.dat',status='old',form='formatted')
  open(35,file='c5.dat',status='old',form='formatted')
  open(36,file='c6.dat',status='old',form='formatted')

  nmain = 6
  naux = 6

  ! iterate over coil files
  do i=1,nmain
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
        coil_set%main(i,piece,1) = x
        coil_set%main(i,piece,2) = y
        coil_set%main(i,piece,3) = z

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
     coil_set%main_points(i) = piece

     close(filenum)
  enddo !end of all files

  ! All aux coils are in one file, no need to skip
  open(37,file='aux_c.dat',status='old',form='formatted')
  do i=1,naux
     read(37,*) coil_set%aux_points(i)
     do j=1,coil_set%aux_points(i)
        read(37,*) x,y,z
        coil_set%aux(i,j,1) = x
        coil_set%aux(i,j,2) = y
        coil_set%aux(i,j,3) = z
     enddo
  enddo
  close(37)

  call move_coils(coil_set%main, coil_set%main_points, &
       nmain, main_size)
  call move_coils(coil_set%aux, coil_set%aux_points, naux, &
       aux_size)
  !Input the currents into the main coils
  do i=1,nmain * mult_factor
     ! i'm not sure why /14, but it's in konstantin's version
     coil_set%main_current = current/14 
  enddo

  !Currents for auxiliary coils
  do i=1,nmain
     do j=0,mult_factor-1
        coil_set%aux_current(i + (j*nmain)) = current*taper(i)
     enddo
  enddo



  return
end subroutine read_coil_files

! It's harder to generalize this for devices that do not have 
! 4 part symmetry, so i'm not going to bother
! It would also require significantly more calculation
subroutine move_coils(coils, coil_counts, coil_number, piece_count)

  parameter(mult_factor = 8)

  integer :: piece_count, coil_number, coil_index
  real :: coils(coil_number * mult_factor, piece_count, 3)
  integer :: coil_counts(coil_number * mult_factor)


  do i=1,coil_number !loop for the coils
     !update the coil counts
     do coil_index = i,coil_number * mult_factor, coil_number
        coil_counts(coil_index) = coil_counts(i)
     enddo


     do j=1,coil_counts(i)!loop over the individual coils
        !period A', swap x->y, y->x, z->-z
        coil_index = i+coil_number !indices 7-12
        coils(coil_index,j,1) = coils(i,j,2)
        coils(coil_index,j,2) = coils(i,j,1)
        coils(coil_index,j,3) = -coils(i,j,3)

        !period B,  x->y, y->-x, z->z
        coil_index = i+(coil_number*2) !indices 13-18
        coils(coil_index,j,1) = coils(i,j,2)
        coils(coil_index,j,2) = -coils(i,j,1)
        coils(coil_index,j,3) = coils(i,j,3)

        !period B', x->-x, y->y, z->-z
        coil_index = i+(coil_number*3) !indices 19-24
        coils(coil_index,j,1) = -coils(i,j,1)
        coils(coil_index,j,2) = coils(i,j,2)
        coils(coil_index,j,3) = -coils(i,j,3)

        !period C, x->-x, y->-y, z->z
        coil_index = i+(coil_number*4) !indices 25-30
        coils(coil_index,j,1) = -coils(i,j,1)
        coils(coil_index,j,2) = -coils(i,j,2)
        coils(coil_index,j,3) = coils(i,j,3)

        !period C', x->-y, y->-x, z->-z
        coil_index = i+(coil_number*5) !indices 31-36
        coils(coil_index,j,1) = -coils(i,j,2)
        coils(coil_index,j,2) = -coils(i,j,1)
        coils(coil_index,j,3) = -coils(i,j,3)

        !period D, x->-y, y->x, z->z
        coil_index = i+(coil_number*6) !indices 37-43
        coils(coil_index,j,1) = -coils(i,j,2)
        coils(coil_index,j,2) = coils(i,j,1)
        coils(coil_index,j,3) = coils(i,j,3)

        !period D, x->x, y->-y, z->-z
        coil_index = i+(coil_number*7) !indices 44-48
        coils(coil_index,j,1) = coils(i,j,1)
        coils(coil_index,j,2) = -coils(i,j,2)
        coils(coil_index,j,3) = -coils(i,j,3)

     enddo
  enddo

  return
end subroutine move_coils

