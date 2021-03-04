program fibonacci
    implicit none
    integer :: a = 0, b = 1, n
    print "(i1)", a

    do n = 1, 11 ,1
        a = a + b
        b = a - b
        print "(i3)", a
    end do

end program fibonacci