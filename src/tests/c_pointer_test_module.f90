!*****************************************************************************************
!>
!  A test of using `flist` to manage things under the hood
!  for accessing object-oriented Fortran code from C or Python.
!
!@note The idea here is that these routines are
!      meant to be called from C or Python:

    module c_pointer_test_module

    use linked_list_module
    use blah_module
    use iso_c_binding

    implicit none

    private

    type(list) :: list_of_models

    public :: initialize_list
    public :: create_model
    public :: access_model
    public :: destroy_model
    public :: destroy_list

    contains

    subroutine initialize_list() bind(c,name='initialize_list')

    !! List constructor. Call to initialize the
    !! list before we do anything else.

    implicit none

    list_of_models = list(.false.)

    end subroutine initialize_list

    function create_model(i) bind(c,name='create_model') result(cp)

    !! Create a new model and add it to the list.
    !! Return a C pointer that points to the list node.

    implicit none

    integer(c_int),intent(in),value :: i
    type(c_ptr) :: cp

    type(blah),pointer :: s
    type(node),pointer :: n
    class(*),pointer   :: p

    ! create a model:
    allocate(blah :: s)
    s%i = i

    ! now add it to the list as a pointer:
    ! [i is used as the key]
    p => s
    call list_of_models%add_pointer(i,p)

    ! now, let's get a pointer to the list node we just made:
    call list_of_models%get_node(i,n)

    ! return a c pointer to this node:
    cp = c_loc(n)
    !ip = cp_to_ip(c_loc(n))

    end function create_model

    subroutine access_model(cp) bind(c,name='access_model')

    !! Access the node associated with the C pointer.

    implicit none

    type(c_ptr),intent(in) :: cp

    type(node),pointer :: n
    class(*),pointer :: model

    nullify(n)

    ! convert the c pointer back to a fortran
    ! pointer to a node in the list:
    call c_f_pointer(cp, n)

    if (associated(n)) then

        ! get the model from this node:
        call n%get_data(model)

        if (associated(model)) then

            ! do something with the model:
            select type (model)
            type is (blah)
                write(*,*) 'got blah: ', model%i
                model%i = model%i + 1
            class default
                error stop 'error: not the right type'
            end select

        else
            error stop 'model not associated!'
        end if
    else
        error stop 'n not associated!'
    end if

    end subroutine access_model

    subroutine destroy_model(cp) bind(c,name='destroy_model')

    !! Destroy a model and remove it from the list.

    implicit none

    type(c_ptr),intent(in) :: cp

    type(node),pointer :: n

    ! convert the c pointer back to a fortran
    ! pointer to a node in the list:
    call c_f_pointer(cp, n)

    ! remove this model from the list:
    call list_of_models%remove_by_pointer(n)

    end subroutine destroy_model

    subroutine destroy_list() bind(c,name='destroy_list')

    !! List destructor. Call when we don't need it anymore.

    implicit none

    call list_of_models%destroy()

    end subroutine destroy_list

    pure function cp_to_ip(cp) result(ip)
    !! `c_ptr` to integer
    implicit none
    type(c_ptr),intent(in) :: cp
    integer(c_intptr_t) :: ip
    ip = transfer(cp,ip)
    end function cp_to_ip

    pure function ip_to_cp(ip) result(cp)
    !! integer to `c_ptr`
    implicit none
    integer(c_intptr_t),intent(in) :: ip
    type(c_ptr) :: cp
    cp = transfer(ip,cp)
    end function ip_to_cp

    end module c_pointer_test_module
!*****************************************************************************************
