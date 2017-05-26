!*****************************************************************************************
!>
!  Test of [[c_pointer_test_module]].

    program c_pointer_test

    use c_pointer_test_module
    use iso_c_binding

    implicit none

    type(c_ptr) :: cp
    integer :: i !! counter

    call initialize_list()

    call create_model(989,cp)
    do i=1,10
        call access_model(cp)
    end do
    call destroy_model(cp)

    call destroy_list()

    end program c_pointer_test
!*****************************************************************************************
