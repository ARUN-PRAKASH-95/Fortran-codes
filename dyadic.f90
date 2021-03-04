      program dyadic
       implicit none
       integer, dimension(3,3) :: a,d
	   integer, dimension(3,3,3,3) :: c
	   integer :: n = 3,i,j,k,l
	   a = transpose(reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(a)))
       
	   

#############  Dyadic product of two second order tensors #############	   
	   
        do i = 1,n
		   do j = 1,n
			   do k=1,n
				   do l=1,n
						c(i,j,k,l) = a(i,j)*a(k,l)
				   end do
                end do
		   end do
	   end do
    
      
	   
	   do i = 1,n
		   do j = 1,n
			   do k = 1,n
				   do l = 1,n
						print "(i3)", c(i,j,k,l)
				   end do
                end do
		   end do
	   end do	   
	   

#######  Double contraction of 4th and 2nd order tensor 
	  

		do i = 1,n
		   do j = 1,n
			   do k=1,n
				   do l=1,n
						d(i,j) = d(i,j) + c(i,j,k,l)*a(k,l)
				   end do
                end do
		   end do
		end do
	   
	   
		
		do i=1,n
		   do j=1,n
			   print "(i4)", d(i,j)
		   end do
       end do
       

    end program dyadic
    
    
          

    
    
    
    
            

        

        
 


    

 


        


    
    
    
    
  
    
    
    
    
    

    
