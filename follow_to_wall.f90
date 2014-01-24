! program to follow one point until it hits the wall

program follow_to_wall

  use points_module

  real,dimension(3) :: p
  integer :: i,j,isin, inside_vessel,outfile
  real :: dphi, totcur, dist

  call read_input()


  ! get the points
  call get_points()
  outfile = 1
  
  allocate(conn_length(points_number))
  conn_length=0.0

  !file to write output
  open (unit=outfile,file=trim(adjustl(results_file)),status='unknown')
  !write (1,'(3(F9.6,2X))') p(1:3)

  

  do i=1,n_iter
     if (i == 1) then
        points_move(:,:) = points_start(:,:)
     endif
     do j=1,points_number
        ! Skip points that already hit
        if (points_hit_vessel(j) == 1) then
           cycle
        else if (points_hit_divertor(j) == 1) then
        	cycle
        else if (points_hit_limiter(j) == 1) then
        	cycle	   
        endif
      
        ! check if the last move left us inside the vessel
        p = points_move(j,:)
        !isin = inside_vessel(p(1),p(2),p(3),vessel,vessel_size)
        !if (isin == 0) then
           ! Set point to hit
        !   points_hit(j) = 1
        !   cycle        
        !endif
        
        ! for some reason i can't understand removing this
        ! print statement causes the field line follower
        ! to fail.  I need to track this down, but it's
        ! hard to debug without a print statement!
        ! print *,points_move(j,:)
        ! set the current point
        current_point = j

        write (*,'(3(F10.7,2X))'),points_move(j,:)
        call follow_field(points_move(j,:), points_dphi, dist)
        
        conn_length(j)=conn_length(j)+dist

        
     
        ! write the new point
        !write (1,'(3(F9.6,2X))') p(1:3)
     enddo
  enddo

  call record_output(outfile)



  call dealloc_points()
  call deallocate_coils()


end program follow_to_wall

subroutine record_output(filenum)
  use points_module
  use limiter_module
  use div_module
  use vessel_module
  implicit none
  
  integer :: j,k,filenum
  
  do j=1,points_number
     write (filenum,*) 'point number',j
     write (filenum,'(A,3(F9.6,2X))') 'start: ',points_start(j,:)
     write (filenum,'(A,3(F9.6,2X))') 'end:   ',points_end(j,:)
     write (filenum,'(A,3(F9.6,2X))') 'connection length:', conn_length(j)
     if (num_vessels.gt.0) then
        write (filenum,*) 'hit wall:',points_hit_vessel(j)
     end if
     if (num_divertors.gt.0) then
        write (filenum,*) 'hit divertor:', points_hit_divertor(j)
     end if
     if (num_limiters.gt.0) then
        write (filenum,*) 'hit limiter:', points_hit_limiter(j)
     end if
     write (filenum,*) '------------------'
  enddo  
  
end subroutine record_output
