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
  
  do i=1,points_number
     read (3,*) points_start(i,:)
     points_move(i,:) = points_start(i,:)
  enddo

end subroutine get_points

subroutine dealloc_points()
  use points_module

  deallocate(points_start)
  deallocate(points_move)
  deallocate(points_hit_vessel)
  deallocate(points_hit_divertor)
  deallocate(points_hit_limiter)

end subroutine dealloc_points
  

