program flf

  use options_module
  implicit none

  call read_namelist('')
  call init_all()
  if (general_option == 0) call comprehensive_test()
  if (general_option == 1) call follow_to_wall()
  if (general_option == 2) call Bmag()
  if (general_option == 3) call eps_eff()
end program flf
