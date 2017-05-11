! program to follow one point until it hits the wall

subroutine eps_eff

  use points_module
  use options_module
  use lcfs_module
  use div_module
  use coil_module
  use eps_module

  double precision,dimension(3) :: p, b, dpsi, pxyz
  integer :: i,j,isin, inside_vessel,outfile, istate
  integer :: axis_index, interp_index
  double precision :: phi, phiin, r, z, axis_phi, rmag, zmag, pi
  double precision :: linear_interpolate
  double precision :: rline, zline, magline, kg, get_kg
  double precision :: dphi, totcur, dist, magb, magp
  double precision :: distance_to_lcfs, dist_lcfs

  ! print *, 'number of LCFS:', num_lcfs

  ! get the points
  call get_eps_points()
  outfile = 1
 
  call allocate_eps(n_iter)
  
  

  !file to write output
  open (unit=outfile,file=trim(adjustl(results_file)),status='unknown')
  !write (1,'(3(F9.6,2X))') p(1:3)

   

  do j=1,points_number
     points_complete(j) = 1
     call clear_eps()
     
     !set the psi values here
     dpsi = (/ points_start(j,2), dble(0.0), dble(0.0) /)

     ! set the current point
     current_point = j
     ! write(*,*),'point number',j

     call pol2cart(points_move(j,:), pxyz)
     call compute_full_bs(pxyz, b)
     magb = (b(1)**2 + b(2)**2 + b(3)**2)**0.5
     magp = (dpsi(1)**2 + dpsi(2)**2 + dpsi(3)**2)**0.5
     kg = get_kg(pxyz, dpsi, b, magb, magp)

     !assign the initial values
     ex(1) = pxyz(1)
     ey(1) = pxyz(2)
     ez(1) = pxyz(3)
     epx(1) = dpsi(1)
     epy(1) = dpsi(2)
     epy(1) = dpsi(3)
     ebmag(1) = magb
     epmag(1) = magp
     ekg(1) = kg


     do i=2,n_iter+1
     	! keep track of number of steps for limiter calculation
     	current_step=i
        
        ! Skip points that already hit
        if (points_hit(j) == 1) then
           cycle
        end if
       
        !call follow_field(points_move(j,:), points_dphi, dist, &
        !     istate)
        !call pol2cart(points_move(j,:), pxyz)
        call follow_field_s_wpsi(pxyz, dpsi, points_dphi, istate)
        call compute_full_bs(pxyz, b)
        
        magb = (b(1)**2 + b(2)**2 + b(3)**2)**0.5
        magp = (dpsi(1)**2 + dpsi(2)**2 + dpsi(3)**2)**0.5
        kg = get_kg(pxyz, dpsi, b, magb, magp)
        !assign the eps values
        ex(i) = pxyz(1)
        ey(i) = pxyz(2)
        ez(i) = pxyz(3)
        epx(i) = dpsi(1)
        epy(i) = dpsi(2)
        epy(i) = dpsi(3)
        ebmag(i) = magb
        epmag(i) = magp
        ekg(i) = kg
        
        call cart2pol(pxyz, points_move(j,:))
                
        !write (*,*) 'istate',istate
        if (istate < 0) then           
           points_complete(j) = 0
           exit
        end if

        !for writing B field
        call pol2cart(points_move(j,:), pxyz)       
        !write (*,'(4(F15.7,2X))'),points_move(j,:), magb
     
        
        

     enddo
     call calc_eps(500,300)

  enddo

  


  call dealloc_points()
  call deallocate_coils()
  call deallocate_eps()


end subroutine eps_eff

subroutine calc_eps(numbp, numind)
  use eps_module
  use points_module
  
  implicit none
  integer :: n, numbp, i, j, numind, num_inter
  integer :: licount, ricount
  double precision :: B0, ds, I1, I2, hmin, hmax, dbp, T1, T2, pi, R0
  double precision :: epseff, epseff32
  double precision, dimension(n_iter) :: Bhalf, magPhalf
  double precision, dimension(n_iter+1) :: h
  double precision, dimension(numbp) :: bp
  double precision, dimension(numbp-1) :: sum_H2_I
  integer, dimension(numind) :: ind_left, ind_right
  ! the nemov H and I functions
  double precision, dimension(:), allocatable :: nemH, nemI
  double precision, dimension(:), allocatable :: bmag_int, pmag_int, kg_int

  n = n_iter
  pi = 3.14159265359
  R0 = 1.2

  B0 = sum(ebmag)/(n+1)
  Bhalf = 0.5*(ebmag(2:n+1) + ebmag(1:n))
  magPhalf = 0.5*(epmag(2:n+1) + epmag(1:n))
  ds = points_dphi

  I1 = sum(ds/Bhalf)
  I2 = (1./sum(ds*magPhalf/Bhalf))**2
  h = ebmag/B0
  hmin = minval(h)
  hmax = maxval(h)
  dbp = (hmax - hmin)/(numbp+1)
  bp(1) = hmin
  do i=2,numbp
     bp(i) = bp(i-1) + dbp
  end do

  !loop over all values of bp
  do i=2,numbp-1
     !we need to find all the indices where bmag crosses a given value
     ind_left = 0
     ind_right = 0
     licount = 0
     ricount = 0
     !this is slow...
     do j = 1,n-1
        if (h(j) >= bp(i) .and. h(j+1) < bp(i)) then
           licount = licount + 1
           ind_left(licount) = j
        end if
        if (h(j) <= bp(i) .and. h(j+1) > bp(i)) then
           ricount = ricount + 1
           ind_right(ricount) = j
        end if
        if (licount == numind .or. ricount == numind) then
           exit
        end if
     end do
     
     ! we didn't find any points
     if (licount == 0 .or. ricount == 0) then
        cycle
     end if
     !fix it so the endpoint pairs match
     if (ind_left(1) > ind_right(1)) then
        if (licount == 1 .or. ricount == 1) then
           cycle
        end if
        ind_right = cshift(ind_right, 1)
        ricount = ricount - 1
     end if
     if (ind_left(licount) > ind_right(ricount)) then
        licount = licount - 1
     end if

     if (licount /= ricount) then
        cycle
     end if
     ! allocate the nemov H and I functions
     allocate(nemH(licount))
     allocate(nemI(licount))
     do j = 1,licount
        if (ind_right(j) <= ind_left(j) + 1) then
           !the points are too close together to do anything
           nemH(j) = 0
           nemI(j) = dble(0.00001)
           !these are the dummy values that Canik used
        else
           num_inter = ind_right(j) - ind_left(j) + 1
           allocate(bmag_int(num_inter))
           allocate(pmag_int(num_inter))
           allocate(kg_int(num_inter))

           bmag_int = ebmag(ind_left(j)+1:ind_right(j))
           pmag_int = epmag(ind_left(j)+1:ind_right(j))
           kg_int = ekg(ind_left(j)+1:ind_right(j))
           
           !terms of the integration, can clean up in terms of h not bmag
           T1 = (1./bp(i))
           T2 = sum((ds/bmag_int)*sqrt(bp(i) - (bmag_int)/B0) * &
                (4*(B0/bmag_int)-1/bp(i)) * pmag_int*kg_int)
           nemH(j) = T1*T2
           nemI(j) = sum((ds/bmag_int)*sqrt(1 - (bmag_int/(B0 * bp(i)))))

           deallocate(bmag_int)
           deallocate(pmag_int)
           deallocate(kg_int)
        end if
     end do
   
     sum_H2_I(i) = sum((nemH**2)/nemI)
     deallocate(nemH)
     deallocate(nemI)
  end do
  
  epseff32 = (pi*R0**2/(8*sqrt(2.)))*(I1*I2*sum(dbp*sum_H2_I))
  epseff = epseff32**(2./3)
  !write(*,*) 'epseff',epseff,'epseff32',epseff32
  write(*,*) epseff, epseff32


end subroutine calc_eps


subroutine allocate_eps(n)
  use eps_module
  
  implicit none
  integer :: n

  allocate(ex(n+1))
  allocate(ey(n+1))
  allocate(ez(n+1))
  allocate(epx(n+1))
  allocate(epy(n+1))
  allocate(epz(n+1))
  allocate(ebmag(n+1))
  allocate(epmag(n+1))
  allocate(ekg(n+1))
end subroutine allocate_eps

subroutine clear_eps()
  use eps_module
  implicit none

  ex = 0.
  ey = 0.
  ez = 0.
  epx = 0.
  epy = 0.
  epz = 0.
  ebmag = 0.
  epmag = 0.
  ekg = 0.
end subroutine clear_eps

subroutine deallocate_eps()
  use eps_module
  implicit none

  deallocate(ex)
  deallocate(ey)
  deallocate(ez)
  deallocate(epx)
  deallocate(epy)
  deallocate(epz)
  deallocate(ebmag)
  deallocate(epmag)
  deallocate(ekg)
end subroutine deallocate_eps
  
double precision function get_kg(pxyz, dpsi, b, magb, magp)

  implicit none
  double precision, dimension(3) :: dmagb !derivatives of magb with respect to xyz
  double precision, dimension(3,3) :: dh !derivatives of components of h with respect to xyz
  double precision, dimension(3,3) :: db
  double precision, dimension(3) :: pxyz, dpsi, b, hdotdelh, hcross, h
  double precision :: magb, magp, dot

  call gradb(pxyz, db)

  h = b/magb
  !calculate db
  dmagb(1) = (1./magb)*dot(b,db(:,1))
  dmagb(2) = (1./magb)*dot(b,db(:,2))
  dmagb(3) = (1./magb)*dot(b,db(:,3))
  
  !calculate dh
  dh(:,1) = (1./magb)*db(:,1) - (dmagb(1) / magb**2)*b
  dh(:,2) = (1./magb)*db(:,2) - (dmagb(2) / magb**2)*b
  dh(:,3) = (1./magb)*db(:,3) - (dmagb(3) / magb**2)*b

  hdotdelh(1) = dot(h, dh(1,:))
  hdotdelh(2) = dot(h, dh(2,:))
  hdotdelh(3) = dot(h, dh(3,:))

  call cross_product(h, hdotdelh, hcross)
  get_kg = (1./magP)*dot(hcross,dpsi) 
  
  !debug section, spotchecked against matlab
  ! write(*,*) 'pxyz', pxyz
  ! write(*,*) 'b', b
  ! write(*,*) 'magb', magb
  ! write(*,*) 'magp', magp
  ! write(*,*) 'dpsi',dpsi
  ! write(*,*) 'dbx',db(1,:)
  ! write(*,*) 'dby',db(2,:)
  ! write(*,*) 'dbz',db(3,:)
  ! write(*,*) 'dhx',dh(1,:)
  ! write(*,*) 'dhy',dh(2,:)
  ! write(*,*) 'dhz',dh(3,:)  
  ! write(*,*) 'hdotdelh',hdotdelh
  ! write(*,*) 'hcross',hcross
  ! write(*,*) 'kg',kg

end function get_kg



  
  
