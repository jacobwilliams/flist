!*****************************************************************************************
!> author: Jacob Williams
!
!  A key to a linked list

    module key_module

    use iso_fortran_env

    implicit none

    private

    type,abstract,public :: key_class
        !! Can be used as a key for the list.
        !! it can be extended to use any data as a key.
        !! all that is necessary is to define the == operator function.
        !! For convienence, integer or characters keys are also
        !! allowed to be used.
    contains
        !private  ! remove for now to work around intel compile bug. see issue #6
        procedure(key_equal_func),deferred :: key_equal
        generic,public :: operator(==) => key_equal
    end type key_class

    abstract interface
        pure elemental logical function key_equal_func(item1,item2)
            !! interface for equality operator for [[key_class]].
            import :: key_class
            implicit none
            class(key_class),intent(in) :: item1
            class(key_class),intent(in) :: item2
        end function key_equal_func
    end interface

!*****************************************************************************************
    end module key_module
!*****************************************************************************************
