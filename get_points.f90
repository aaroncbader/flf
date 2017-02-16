! read file and assign points

subroutine get_points()
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
     read (3,*) points_start(i,:)
     points_move(i,:) = points_start(i,:)
  enddo

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

subroutine dealloc_points()
  use points_module

  deallocate(points_start)
  deallocate(points_move)
  deallocate(points_hit)
  deallocate(points_hit_vessel)
  deallocate(points_hit_divertor)
  deallocate(points_hit_limiter)

end subroutine dealloc_points
  

