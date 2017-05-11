! read file and assign points

subroutine get_points()
  use points_module
  implicit none
  integer :: i
  integer :: minperproc, overflow, prevend

  !character, dimension(:) :: filename
  open (unit=3,file=trim(adjustl(points_file)),action='read')
  
  ! allocate everything
  allocate(points_start(points_number, 3))
  allocate(points_move(points_number, 3))
  allocate(points_end(points_number, 3))
  allocate(points_hit(points_number))
  allocate(points_complete(points_number))
  allocate(startinds(num_procs))
  allocate(endinds(num_procs))
  points_hit(:) = 0
  points_end(:,:) = 0
  points_complete(:) = 0

  do i=1,points_number
     read (3,*) points_start(i,:)
     points_move(i,:) = points_start(i,:)
  enddo

  minperproc = points_number/num_procs
  overflow = points_number - (num_procs * minperproc)
  prevend = 0
  !for now read everything in to all of them
  do i = 1,num_procs
     startinds(i) = prevend + 1
     if (overflow > 0) then
        endinds(i) = startinds(i) + minperproc
        overflow = overflow - 1
     else
        endinds(i) = startinds(i) + minperproc - 1
     end if
     prevend = endinds(i)
  end do
  points_ind_begin = startinds(my_pn+1)
  points_ind_end = endinds(my_pn+1)

end subroutine get_points

!a slight adaptation for getting points for eps_eff following
!only read in two values, first is r on midplane, second is dpsi
subroutine get_eps_points()
  use points_module
  implicit none
  integer :: i

  !character, dimension(:) :: filename
  open (unit=3,file=trim(adjustl(points_file)),action='read')
  
  ! allocate everything
  allocate(points_start(points_number, 3))
  allocate(points_move(points_number, 3))
  allocate(points_end(points_number, 3))
  allocate(points_hit(points_number))
  allocate(points_complete(points_number))
  points_hit(:) = 0

  do i=1,points_number
     read (3,*) points_start(i,1:2)
     points_move(i,1) = points_start(i,1)
     points_move(i,2) = 0.0
     points_move(i,3) = 0.0
  enddo

end subroutine get_eps_points

!store mpi points
subroutine gather_results
  use points_module
  use limiter_module, only: num_limiters
  use div_module, only: num_divertors
  use vessel_module, only: num_vessels

  implicit none
  include 'mpif.h'
  integer :: i, s1, ierr, pi, j, k, t, status(mpi_status_size)
  double precision :: p
  integer :: m, tag
  double precision, dimension(:,:), allocatable :: temp_points
  double precision, dimension(:), allocatable :: temp_lc
  integer, dimension(:), allocatable :: temp_hits

  call mpi_barrier(mpi_comm_world, ierr)

  if (my_pn == 0) then
     do i = 2,num_procs
        pi = i-1
        s1 = endinds(i) - startinds(i) + 1
        
        allocate(temp_points(s1, 3))
        allocate(temp_hits(s1))
        allocate(temp_lc(s1))
        temp_hits = 0
        temp_lc = 0.
        temp_points = 0.
        
        
        call mpi_recv(temp_hits, s1, MPI_INTEGER, pi, 10+100*pi, &
             MPI_COMM_WORLD, status, ierr)
        points_complete(startinds(i): endinds(i)) = temp_hits

        call mpi_recv(temp_points, s1*3, MPI_REAL8, pi, 11+100*pi, &
             MPI_COMM_WORLD, status, ierr)
        points_end(startinds(i):endinds(i), :) = temp_points
        
        
        call mpi_recv(temp_points, s1*3, MPI_REAL8, pi, 12+100*pi, &
             MPI_COMM_WORLD, status, ierr)
        points_move(startinds(i):endinds(i), :) = temp_points
    
       
        call mpi_recv(temp_lc, s1, MPI_REAL8, pi, 13+100*pi, &
             MPI_COMM_WORLD, status, ierr)
        conn_length(startinds(i):endinds(i)) = temp_lc
        
        if (num_vessels > 0) then
           call mpi_recv(temp_hits, s1, MPI_INTEGER, pi, 14+100*pi, &
             MPI_COMM_WORLD, status, ierr)
           points_hit_vessel(startinds(i):endinds(i)) = temp_hits
        end if
        if (num_limiters > 0) then
           call mpi_recv(temp_hits, s1, MPI_INTEGER, pi, 15+100*pi, &
             MPI_COMM_WORLD, status, ierr)
           points_hit_limiter(startinds(i):endinds(i)) = temp_hits
        end if
        if (num_divertors > 0) then
           call mpi_recv(temp_hits, s1, MPI_INTEGER, pi, 16+100*pi, &
             MPI_COMM_WORLD, status, ierr)
           points_hit_divertor(startinds(i):endinds(i)) = temp_hits
        end if
           
        deallocate(temp_points)
        deallocate(temp_hits)
        deallocate(temp_lc)
     end do
     
  else
     i = my_pn+1
     s1 = endinds(i) - startinds(i) + 1
     allocate(temp_points(s1, 3))
     allocate(temp_hits(s1))
     allocate(temp_lc(s1))

     temp_hits = points_complete(startinds(i):endinds(i))
     call mpi_send(temp_hits, s1, MPI_INTEGER, 0, 10+100*my_pn, &
          MPI_COMM_WORLD, ierr)
     

     temp_points = points_end(startinds(i):endinds(i), :)
     call mpi_send(temp_points, s1*3, MPI_REAL8, 0, 11+100*my_pn, &
          MPI_COMM_WORLD, ierr)
     temp_points = points_move(startinds(i):endinds(i), :)
     call mpi_send(temp_points, s1*3, MPI_REAL8, 0, 12+100*my_pn, &
          MPI_COMM_WORLD, ierr)
     temp_lc = conn_length(startinds(i):endinds(i))
     
     call mpi_send(temp_lc, s1, MPI_REAL8, 0, 13+100*my_pn, &
          MPI_COMM_WORLD, ierr)
     if (num_vessels > 0) then
        temp_hits = points_hit_vessel(startinds(i):endinds(i))
        call mpi_send(temp_points, s1, MPI_INTEGER, 0, 14+100*my_pn, &
             MPI_COMM_WORLD, ierr)
     end if
     if (num_limiters > 0) then
        temp_hits = points_hit_limiter(startinds(i):endinds(i))
        call mpi_send(temp_points, s1, MPI_INTEGER, 0, 15+100*my_pn, &
             MPI_COMM_WORLD, ierr)
     end if
     if (num_divertors > 0) then
        temp_hits = points_hit_divertor(startinds(i):endinds(i))
        call mpi_send(temp_points, s1, MPI_INTEGER, 0, 16+100*my_pn, &
             MPI_COMM_WORLD, ierr)
     end if
     
     deallocate(temp_points)
     deallocate(temp_hits)
     deallocate(temp_lc)
  end if
  call mpi_barrier(mpi_comm_world, ierr)
 
 
end subroutine gather_results


subroutine dealloc_points()
  use points_module

  deallocate(points_start)
  deallocate(points_move)
  if (allocated(points_hit)) deallocate(points_hit)
  if (allocated(points_hit_vessel)) deallocate(points_hit_vessel)
  if (allocated(points_hit_divertor)) deallocate(points_hit_divertor)
  if (allocated(points_hit_limiter)) deallocate(points_hit_limiter)

end subroutine dealloc_points
  

