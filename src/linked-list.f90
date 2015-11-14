!*****************************************************************************************
!> author: Jacob Williams
!  date: 11/13/2015
!  license: BSD
!
! A generic list.
!
! It uses an unlimited polymorphic `class(*)` pointer variable to allow it
! to contain any type of data. The *key* can be an integer or string.
    
    module linked_list_module

    use iso_fortran_env

    implicit none

    private

    type :: node

        !! a node in the linked list.

        private

        class(*),allocatable :: key  !! the key (can be integer or string)

        class(*),pointer :: value => null()  !! the data to hold

        logical :: destroy_on_delete = .true. !! if true, value pointer is deallocated
                                              !! when it is removed from the list,
                                              !! or the list is destroyed. If false,
                                              !! it is only nullified.

        type(node),pointer :: next     => null()  !! the next one in the list
        type(node),pointer :: previous => null()  !! the previous one in the list

        contains

        private

        procedure :: destroy => destroy_node_data  !! deallocate value

    end type node

    type,public :: list

        !! linked list of pointers to polymorphic types.

        private

        logical :: case_sensitive = .true. !! key lookup is case sensitive
        integer :: count          = 0      !! number of items in the list

        type(node),pointer :: head => null() !! the first item in the list
        type(node),pointer :: tail => null() !! the last item in the list

        contains

        private

        procedure,public :: add                     !! add an item to the list
        procedure,public :: get => get_data         !! get a pointer to an item in the list
        procedure,public :: destroy => destroy_list !! destroy the list and
                                                    !! deallocate/finalize all the data
        procedure,public :: has_key                 !! if the key is present in the list
        procedure,public :: print_keys              !! print all the keys in the list (debugging)

        generic,public :: remove => remove_by_key,remove_by_pointer !! remove item from the list

        !private routines:
        procedure :: traverse          !! traverse each node of the list
        procedure :: remove_by_key     !! remove node given the key
        procedure :: remove_by_pointer !! remove node given pointer
        procedure :: get_node          !! get a pointer to a node in the list
        procedure :: keys_equal        !! for testing key string equality

    end type list

    interface list
        procedure :: initialize_list !! constructor for a list
    end interface

    abstract interface
        subroutine iterator_func(me,done) !! for traversing all nodes in a list
        import :: node
        implicit none
        type(node),pointer :: me
        logical,intent(out) :: done !! set to true to stop traversing
        end subroutine iterator_func
    end interface

    contains
!*****************************************************************************************

!*****************************************************************************************
    function has_key(me,key)

    !! Returns true if the key is present in the list

    implicit none

    class(list),intent(inout) :: me
    class(*),intent(in)       :: key
    logical                   :: has_key

    has_key = .false.

    ! traverse the list:
    call me%traverse(key_search)

    contains

        subroutine key_search(p,done)  !! search for the key
        implicit none
        type(node),pointer  :: p
        logical,intent(out) :: done
        has_key = me%keys_equal(p%key,key)
        done = has_key
        end subroutine key_search

    end function has_key
!*****************************************************************************************

!*****************************************************************************************
    function initialize_list(case_sensitive) result(lst)

    !! list constructor.

    implicit none

    type(list) :: lst
    logical,intent(in) :: case_sensitive !! if true, then string key searches are case sensitive.

    lst%case_sensitive = case_sensitive

    end function initialize_list
!*****************************************************************************************

!*****************************************************************************************
    subroutine traverse(me,iterator)

    !! traverse list from head to tail, and call the iterator function for each node.

    implicit none

    class(list),intent(inout) :: me
    procedure(iterator_func)  :: iterator  !! the function to call for each node.

    type(node),pointer :: p
    logical :: done

    done = .false.
    p => me%head

    do
        if (associated(p)) then
            call iterator(p,done)
            if (done) exit
            p => p%next
        else
            exit ! done
        end if
    end do

    end subroutine traverse
!*****************************************************************************************

!*****************************************************************************************
    subroutine destroy_node_data(me)

    !! destroy the data in the node.

    implicit none

    class(node),intent(inout) :: me

    if (allocated(me%key)) deallocate(me%key)

    if (me%destroy_on_delete) then
        ! deallocates the pointer (and call any finalizer)
        ! (otherwise, it is up to the caller to do this)
        if (associated(me%value)) deallocate(me%value)
    end if

    nullify(me%value)

    end subroutine destroy_node_data
!*****************************************************************************************

!*****************************************************************************************
    subroutine destroy_list(me)

    !! destroy the list (traverses from head to tail)

    implicit none

    class(list),intent(inout) :: me

    me%count = 0

    if (associated(me%head)) call destroy_node(me%head)

    nullify(me%head)
    nullify(me%tail)

    end subroutine destroy_list
!*****************************************************************************************

!*****************************************************************************************
    recursive subroutine destroy_node(me)

    !! destroy the node (and subsequent ones in the list).

    implicit none

    type(node),pointer :: me

    if (associated(me)) then
        call me%destroy()
        call destroy_node(me%next)
        nullify(me%previous)
        deallocate(me)
        nullify(me)
    end if

    end subroutine destroy_node
!*****************************************************************************************

!*****************************************************************************************
    subroutine remove_by_key(me,key)

    !! Remove an item from the list (given the key).

    implicit none

    class(list),intent(inout)   :: me
    character(len=*),intent(in) :: key

    type(node),pointer :: p

    call me%get_node(key,p)
    call me%remove_by_pointer(p)

    end subroutine remove_by_key
!*****************************************************************************************

!*****************************************************************************************
    subroutine remove_by_pointer(me,p)

    !! Remove an item from the list.

    implicit none

    class(list),intent(inout) :: me
    type(node),pointer        :: p   !! the item to remove

    logical :: has_next, has_previous

    if (associated(p)) then

        call p%destroy()  ! destroy the data

        has_next     = associated(p%next)
        has_previous = associated(p%previous)

        if (has_next .and. has_previous) then    !neither first nor last in a list
            p%previous%next => p%next
            p%next%previous => p%previous
        elseif (has_next .and. .not. has_previous) then    !first one in a list
            me%head          => p%next
            me%head%previous => null()
        elseif (has_previous .and. .not. has_next) then    !last one in a list
            me%tail      => p%previous
            me%tail%next => null()
        elseif (.not. has_previous .and. .not. has_next) then  !only one in the list
            me%head => null()
            me%tail => null()
        end if

        deallocate(p)
        nullify(p)

        me%count = me%count - 1

    end if

    end subroutine remove_by_pointer
!*****************************************************************************************

!*****************************************************************************************
    subroutine print_keys(me)

    !! Prints a list of the keys to the console (for debugging).

    implicit none

    class(list),intent(inout) :: me

    ! traverse the list:
    call me%traverse(print_key)

    contains

        subroutine print_key(p,done)  !! print the key for this node
        implicit none
        type(node),pointer :: p
        logical,intent(out) :: done
        
        associate (key => p%key)
            select type (key)
            type is (character(len=*))
                write(output_unit,'(A)') key
            type is (integer)
                write(output_unit,'(I0)') key
            end select
        end associate
        done = .false.
        end subroutine print_key

    end subroutine print_keys
!*****************************************************************************************

!*****************************************************************************************
    subroutine get_data(me,key,value)

    !! Returns a pointer to the data stored in the list.

    implicit none

    class(list),intent(in)       :: me
    class(*),intent(in)          :: key
    class(*),pointer,intent(out) :: value

    type(node),pointer :: p

    call me%get_node(key,p)
    if (associated(p)) then
        value => p%value
    else
        value => null()
    end if

    end subroutine get_data
!*****************************************************************************************

!*****************************************************************************************
    subroutine get_node(me,key,p_node)

    !! Returns a pointer to a node in a list.

    implicit none

    class(list),intent(in)         :: me
    class(*),intent(in)            :: key
    type(node),pointer,intent(out) :: p_node

    type(node),pointer :: p

    nullify(p_node)

    p => me%head
    do
        if (associated(p)) then
            if (me%keys_equal(p%key,key)) then
                p_node => p
                return
            end if
            p => p%next
        else
            return !not found
        end if
    end do

    end subroutine get_node
!*****************************************************************************************

!*****************************************************************************************
    function keys_equal(me,k1,k2)

    !! Returns true if the two keys are equal.
    !!
    !! Allowing a key to be an integer or a character string
    !! (can be case sensitive or not).

    implicit none

    class(list),intent(in) :: me
    class(*),intent(in)    :: k1
    class(*),intent(in)    :: k2
    logical                :: keys_equal
    
    keys_equal = .false.
    
    if (same_type_as(k1,k2)) then
    
        select type (k1)
        type is (integer)
        
            select type (k2)
            type is (integer)
                keys_equal = k1 == k2
            end select
            
        type is (character(len=*))
        
            select type (k2)
            type is (character(len=*))
                if (me%case_sensitive) then
                    keys_equal = k1 == k2
                else
                    keys_equal = uppercase(k1) == uppercase(k2)
                end if
            end select
            
        end select
        
    end if

    end function keys_equal
!*****************************************************************************************

!*****************************************************************************************
    pure function uppercase(str) result(string)

    !! Convert a string to uppercase.

    implicit none

    character(len=*),intent(in) :: str
    character(len=len(str))     :: string

    integer :: i,idx

    character(len=*),parameter :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=*),parameter :: lower = 'abcdefghijklmnopqrstuvwxyz'

    string = str
    do i = 1, len_trim(str)
        idx = index(lower, str(i:i))
        if (idx > 0) string(i:i) = upper(idx:idx)
    end do

    end function uppercase
!*****************************************************************************************

!*****************************************************************************************
    subroutine add(me,key,value,destroy_on_delete)

    !! Add an item to end of the list, and associate its pointer to the input value.
    !!
    !!@note If an item with the same key is already in the list,
    !!      it is removed and the new one will replace it.

    implicit none

    class(list),intent(inout)   :: me
    class(*),intent(in)         :: key
    class(*),intent(in),pointer :: value  !! *value* is unlimited polymorphic, so it can
                                          !! be any scalar type. If the type includes
                                          !! pointers or other objects that must be
                                          !! cleaned up when it is destroyed, then it
                                          !! should include a finalizer.
    logical,intent(in),optional :: destroy_on_delete !! If false, the finalizer will
                                                     !! not be called when the item is
                                                     !! removed from the list (the
                                                     !! pointer will only be
                                                     !! nullified, so the caller is
                                                     !! responsible for cleaning it up
                                                     !! to avoid memory leaks).
                                                     !! The default is *True*.

    type(node),pointer :: p

    !only allowing integer or string keys:
    select type (key)
    type is (integer)
        !ok
    type is (character(len=*))
        !ok
    class default
        error stop 'Error: key must be an integer or character string'
    end select

    ! if the node is already there, then remove it
    call me%get_node(key,p)
    if (associated(p)) call me%remove(p)

    if (associated(me%tail)) then
        allocate(me%tail%next)  !insert new item at the end
        p => me%tail%next
        p%previous => me%tail
    else
        allocate(me%head)  !first item in the list
        p => me%head
    end if

    me%tail  => p
    me%count =  me%count + 1

    allocate(p%key, source = key)
    p%value  => value

    if (present(destroy_on_delete)) then
        p%destroy_on_delete = destroy_on_delete
    end if

    end subroutine add
!*****************************************************************************************

!*****************************************************************************************
    end module linked_list_module
!*****************************************************************************************