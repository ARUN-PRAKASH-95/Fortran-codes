      program multidim
       implicit none
       integer, dimension(5,5,5) :: a
       integer :: n = 5,i,j,k
	
       do i = 1, n
          do j = 1, n
		    do k = 1,n   
			    a(i,j,k) = 1
		    end do
	    end do
      end do

      do i = 1, n
	    do j = 1, n
		    do k=1,n   
			    print "(i3)", a(i,j,k)
		    end do
	    end do
      end do
    
    

    end program multidim
