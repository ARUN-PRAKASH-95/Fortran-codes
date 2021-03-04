      program double_contr
       implicit none
       integer, dimension(3,3) :: a
	   integer :: n = 3,i,j, c = 0
	   a = transpose(reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(a)))
       
	   do i = 1,n
		   do j=1,n
			   print "(i3)", a(i,j)
		   end do
	   end do
	   
	   do i = 1,n
		   do j = 1,n
			   c = c + a(i,j)*a(i,j)
		   end do
	   end do
    
       print "(i3)", c

      end program double_contr