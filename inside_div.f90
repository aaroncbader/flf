
! Open up the beginning of all the EMC3 divertor files and read out the sizes.
! Then allocate a large divertor array which includes all the information and
! is sized to the largest divertor.

! There are large inefficiencies if one divertor is smaller than others
subroutine alloc_div()

  use div_module

  implicit none

  integer :: i, filenum, tor_points, div_segs, tor_max, seg_max, repeating
  character*72 :: dummy

  filenum = 21

  tor_max = 0
  seg_max = 0


  ! Check to make sure some files were passed
  if (num_divertors .eq. 0) then
     print *,'No divertors to include'
     return
  end if

  ! Allocate the arrays that let us know how many divertor
  ! segments there are for each divertor.
  allocate(div_tor_num(num_divertors))
  allocate(div_seg_num(num_divertors))
  allocate(div_repeat(num_divertors))

  ! Go through all the files and assign the values to the appropriate segments
  do i =1,num_divertors
     open(filenum, file=trim(div_files(i)), status='old', form='formatted')
     ! first line is a dummy variable
     read(filenum,*) dummy
     ! this is all we need
     read(filenum,*) tor_points, div_segs, repeating
     close(filenum)
     
     ! put them into the arrays
     div_tor_num(i) = tor_points
     div_seg_num(i) = div_segs
     div_repeat(i) = repeating
     

     ! set the max values for allocating the divertors
     if (tor_points .gt. tor_max) then
        tor_max = tor_points
     end if
     if (div_segs .gt. seg_max) then
        seg_max = div_segs
     end if
  end do

  if ((seg_max .eq. 0) .or. (tor_max .eq. 0)) then
     print *,'No divertor plates found in the files!'
     num_divertors = 0
     return
  end if
  
  num_divertors = num_divertors
  ! Finally allocate the divertor array
  allocate(divertor(num_divertors, tor_max, seg_max, 2))
  allocate(div_tor_vals(num_divertors, tor_max))

end subroutine alloc_div


! Load in all the div values
subroutine load_div()

  use div_module
  character*72 :: dummy

  integer :: i,j,k,filenum,d1,d2,d3
  real :: t,r,z, d4, d5,pi
  

  pi = 3.1415927
  filenum = 21

  do i = 1,num_divertors
     open(filenum, file=trim(div_files(i)), status='old', form='formatted')
     read(filenum, *) dummy
     ! These are unneeded dummy variables
     read(filenum, *) d1,d2,d3,d4,d5
     do j=1,div_tor_num(i)
        !read in the tor value
        read(filenum,*) t
        div_tor_vals(i,j) = t*pi/180.
        do k=1,div_seg_num(i)
           read(filenum,*) r,z
           ! XXX HACK EMC3 measures in cm, everything else is m
           divertor(i,j,k,1) = r/100
           divertor(i,j,k,2) = z/100
        end do
     end do
     close(filenum)
  end do

end subroutine load_div

subroutine deallocate_div_and_axis()
  use div_module
  if (allocated(div_tor_num)) deallocate(div_tor_num)
  if (allocated(div_seg_num)) deallocate(div_seg_num)
  if (allocated(divertor)) deallocate(divertor)
  if (allocated(div_tor_vals)) deallocate(div_tor_vals)
  if (allocated(mag_axis)) deallocate(mag_axis)
  if (allocated(div_files)) deallocate(div_files)
end subroutine deallocate_div_and_axis

subroutine load_axis()
  use div_module
  
  integer :: filenum, i
  real :: pi

  filenum = 21
  pi = 3.14159

  open(filenum, file=trim(axis_file), status='old', form='formatted')
  read(filenum, *) axis_points
  allocate(mag_axis(axis_points,3))
  
  do i = 1,axis_points
     read(filenum, *) mag_axis(i,1:2)
     ! We calculate te phi value since it's not explicit
     mag_axis(i,3) = (i-1)* (pi/4)/axis_points
  end do

  close(filenum)  

end subroutine load_axis


! This subroutine will calculate if a point is behind a divertor wall.

! The way it works is it first calculates the intermediate divertor segment
! for the desired phi value.  Then it draws a line between the magnetic axis
! and the desired point.  It checks for intersections between the mag axis
! line and each line segment of the divertor.  If the lines intersect, the
! point is behind the wall, if they don't intersect, it is not behind the wall.

! If the divertors are solid boxes, the calculation should be similar to the
! inside vessel calculation, and that one should be used.
integer function inside_div(rin, zin, phiin)
  use div_module
  implicit none

  real :: pi
  real :: rin,zin,phiin,r,z,phi,axis_phi
  real :: rmag, zmag, linear_interpolate
  real :: rseg1, rseg2, zseg1, zseg2, dum1, dum2
  integer :: intersection, i, j, axis_index, div_index, interp_index
  integer :: does_intersect, slice_size, axis_flip
  real, dimension(:), allocatable :: tor_slice, div_slice

  if (num_divertors.le.0) then
     inside_div = 0
     return
  end if

  pi = 3.1415927
 
  do i = 1,num_divertors

     !Move the quadrant appropriately
     if (div_repeat(i).eq.8) then
        call move_to_first_quad(rin, zin, phiin, r, z, phi)
     else
        phi = modulo(phiin, 2 * pi)
        r = rin
        z = zin
     endif

     ! Got to move this inside the loop now, since some divertors may have
     ! different calculation

     ! handle getting the axis for values of phi greater than pi/2
     axis_flip = 1 !deal with stell symmetry
     if (phi > pi / 2) then
        axis_phi = modulo(phi, pi/2)
        if (axis_phi > pi/4) then
           axis_flip = -1
           axis_phi = pi/2 - axis_phi
        endif
     else
        axis_phi = phi
     endif
     axis_index = interp_index(axis_phi, mag_axis(:,3), axis_points)

     rmag = linear_interpolate(axis_phi, mag_axis(:,1), mag_axis(:,3), &
          axis_index)
     rmag = rmag * axis_flip
     zmag = linear_interpolate(axis_phi, mag_axis(:,2), mag_axis(:,3), &
          axis_index)
     zmag = zmag * axis_flip
     ! check that there is a plate in the correct bounds
     if ((phi .le. div_tor_vals(i,1)) .or. & 
          phi .ge. div_tor_vals(i,div_tor_num(i))) then
        cycle
     end if
     ! get index, this is where we really save time with many segmented
     ! divertors.
  
     !Passing subarrays only appears to work when it's the first
     ! index, so this guy doesn't work, we need a slice.
     slice_size = div_tor_num(i)
     allocate(tor_slice(slice_size))
     tor_slice = div_tor_vals(i,:)
     div_index = interp_index(phi, tor_slice, div_tor_num(i))


     do j = 1,div_seg_num(i) - 1
        allocate(div_slice(slice_size))
        ! Get interpolated divertor
        div_slice = divertor(i,:,j,1)
        rseg1 = linear_interpolate(phi, div_slice, tor_slice, div_index)

        div_slice = divertor(i,:,j+1,1)
        rseg2 = linear_interpolate(phi, div_slice, tor_slice, div_index)

        div_slice = divertor(i,:,j,2)
        zseg1 = linear_interpolate(phi, div_slice, tor_slice, div_index)

        div_slice = divertor(i,:,j+1,2)
        zseg2 = linear_interpolate(phi, div_slice, tor_slice, div_index)
        
        deallocate(div_slice)

        does_intersect = intersection(rseg1, zseg1, rseg2, zseg2, &
             rmag, zmag, r, z, dum1, dum2)
        !print *,does_intersect, dum1, dum2
        
        if (does_intersect .eq. 1) then
           inside_div = i
           return
        endif

     end do
     deallocate(tor_slice)

  end do

  ! we checked all the values and none were behind the wall
  inside_div = 0
  return


end function inside_div
