!module for keeping around values for the epsilon effective calculation

module eps_module
  implicit none
  real, dimension(:), allocatable :: ex, ey, ez, epx, epy, epz, ebmag, epmag, ekg
end module eps_module
