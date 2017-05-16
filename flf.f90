program flf

  use options_module, only: general_option
  use points_module, only: my_pn, num_procs, log_file, lf
  implicit none
  include 'mpif.h'

  character*5 :: fn

  integer ierr

  ! set up MPI stuff
  call MPI_INIT(ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_pn, ierr)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, num_procs, ierr)
  !create a custom log file for each processor
  write (fn, '(I5.5)') my_pn
  lf = 1000 + my_pn
  log_file = 'output' // fn // '.log'
  open (unit=lf,file=trim(log_file),status='replace')
  

  call read_namelist('')
  call init_all()
  if (general_option == 0) call comprehensive_test()
  if (general_option == 1) call follow_to_wall()
  if (general_option == 2) call Bmag()
  if (general_option == 3) call eps_eff()
  close(lf)

  call MPI_FINALIZE(ierr)
end program flf
