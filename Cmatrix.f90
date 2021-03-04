      program Cmatrix
       implicit none
       double precision :: E = 210e3, lambda, mu, xK
       integer :: sigy = 200,i,j,k,l,n=3,m=6
       real :: v = 0.33
       real, parameter :: TWO = 2.d0, THREE = 3.d0, HALF = 0.5d0
       integer, dimension(3,3) :: id
       real, dimension(3,3,3,3) :: b, I4sym, P4sym, C
       integer, dimension(1:6) :: ii,jj
       real, dimension(6,6) :: A66
       
       
       ii = (/1,2,3,1,2,1/)
       jj = (/1,2,3,2,3,3/)
       
       
       
       id = transpose(reshape((/ 1, 0, 0, 0, 1, 0, 0, 0, 1 /), shape(id)))
       lambda = E*v /((1 + v)*(1 - TWO*v))
       mu =  E / (2*(1+v))
       xK = E/(3*(1 - (2*v)))
       print "(f11.3)", E
       print "(f11.3)", lambda
       
#############  Dyadic product of two second order identity tensors (second term of the stiffness tensor) #############	   
	   
        do i = 1,n
		   do j = 1,n
			   do k=1,n
				   do l=1,n
                    b(i,j,k,l) = xK*id(i,j)*id(k,l)
				   end do
                end do
		   end do
        end do

####################   First term of the C tensor ########################   
 
        do i = 1,n
		   do j = 1,n
			   do k=1,n
				   do l=1,n
                    I4sym(i,j,k,l) = HALF*(id(i,k)*id(j,l) + id(i,l)*id(j,k))
				   end do
                end do
		   end do
        end do
       
        do i = 1,n
		   do j = 1,n
			   do k=1,n
				   do l=1,n
                    P4sym(i,j,k,l) = 2*mu*(I4sym(i,j,k,l) - (1/3)*(id(i,j)*id(k,l)))
				   end do
                end do
		   end do
        end do        

        
        
        
############  C_matrix #################        
       
        do i = 1,n
		   do j = 1,n
			   do k=1,n
				   do l=1,n
                        C(i,j,k,l) = P4sym(i,j,k,l) + b(i,j,k,l)
				   end do
                end do
		   end do
        end do
    
	   
        do i = 1,n
		   do j = 1,n
			   do k = 1,n
				   do l = 1,n
						print "(f10.2)", C(i,j,k,l)
				   end do
                end do
		   end do
        end do	   

        
        print "()"
        
        do i = 1,m
           do j=1,m
               A66(i,j) = C(ii(i),jj(i),ii(j),jj(j))
           end do
        end do    

        do i = 1,m
           do j=1,m
               print "(f10.2)",A66(i,j)
           end do
       end do
          
     end program Cmatrix