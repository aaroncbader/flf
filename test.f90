program test
   use coil_module

   call set_some_stuff(coil_set)

   do i=1,aux_count
      print *,'entry ',i,coil_set%aux_points(i)
   enddo

end program test

subroutine set_some_stuff(coil_set)
   use coil_module

   print *,'aux_size',aux_size
   do i=1,main_count
      coil_set%aux_points(i) = 10*i
   enddo

   
end subroutine set_some_stuff
