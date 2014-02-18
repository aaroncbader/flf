program test_random_number
  implicit none
  integer :: i
  real :: r
  
  call init_random_seed()
  call random_number(r)

  print *,r

end program test_random_number

!Algorithm stolen from gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
subroutine init_random_seed()
  implicit none
  integer, allocatable :: seed(:)
  integer :: i, n, un, istat, dt(8), pid, t(2), s
  integer(8) :: count, tms


  call random_seed(size = n)
  allocate(seed(n))

  ! Fallback to XOR:ing the current time and pid. The PID is
  ! useful in case one launches multiple instances of the same
  ! program in parallel.
  call system_clock(count)
  if (count /= 0) then
     t = transfer(count, t)
  else
     call date_and_time(values=dt)
     tms = (dt(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000 &
          + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
          + dt(3) * 24 * 60 * 60 * 60 * 1000 &
          + dt(5) * 60 * 60 * 1000 &
          + dt(6) * 60 * 1000 + dt(7) * 1000 &
          + dt(8)
     t = transfer(tms, t)
    
  end if
  s = ieor(t(1), t(2))
  pid = getpid() + 1099279 ! Add a prime
  s = ieor(s, pid)
  if (n >= 3) then
     seed(1) = t(1) + 36269
     seed(2) = t(2) + 72551
     seed(3) = pid
     if (n > 3) then
        seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
     end if
  else
     seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
  end if
  call random_seed(put=seed)

end subroutine init_random_seed
