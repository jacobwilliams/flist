!*****************************************************************************************
    program main

    use linked_list_module
    use blah_module

    implicit none

        type(list) :: list_of_stuff

        type(blah)         :: stuff
        type(blah),pointer :: s  !! a pointer for retrieving from a list
        class(*),pointer   :: p,q

        !initialize:
        write(*,*) '--------initialize list'
        list_of_stuff = list(.false.)

        write(*,*) '--------add to list'

        allocate(p, source = 'Horatio');          call list_of_stuff%add_pointer('name',p)
        allocate(p, source = 21);                 call list_of_stuff%add_pointer('age',p)
        stuff%i = 1; allocate(p, source = stuff); call list_of_stuff%add_pointer('stuff1',p)
        stuff%i = 2; allocate(p, source = stuff); call list_of_stuff%add_pointer('stuff2',p)
        stuff%i = 3; allocate(p, source = stuff); call list_of_stuff%add_pointer('stuff3',p)
        stuff%i = 3; allocate(q, source = stuff); call list_of_stuff%add_pointer('stuff3',q,destroy_on_delete=.false.) !add another with the same name
        stuff%i = 4; allocate(p, source = stuff); call list_of_stuff%add_pointer('stuff4',p)

        stuff%i = 1001; allocate(p, source = stuff); call list_of_stuff%add_pointer(1001,p)
        stuff%i = 1002; allocate(p, source = stuff); call list_of_stuff%add_pointer(1002,p)

        !add some non-pointer variables:
        call list_of_stuff%add_clone('hello','there')
        call list_of_stuff%add_clone(-999,.true.)
        call list_of_stuff%add_clone('999',.false.)

        write(*,*) '--------get from list'

        call list_of_stuff%get('999',p)
        if (associated(p)) then
            select type (p)
            type is (logical)
                write(*,*) '999: ',p
            end select
        end if

        call list_of_stuff%get('age',p)
        if (associated(p)) then
            select type (p)
            type is (integer)
                write(*,*) 'age: ',p
            end select
        end if

        call list_of_stuff%get('name',p)
        if (associated(p)) then
            select type (p)
            type is (character(len=*))
                write(*,*) 'name: '//p
            end select
        end if

        !wrapper routines for the blah type:
        call get_blah_from_list(list_of_stuff,'stuff1',s); write(*,*) 'stuff1: ',s%i
        call get_blah_from_list(list_of_stuff,'stuff2',s); write(*,*) 'stuff2: ',s%i
        call get_blah_from_list(list_of_stuff,'stuff3',s); write(*,*) 'stuff3: ',s%i
        call get_blah_from_list(list_of_stuff,'stuff4',s); write(*,*) 'stuff4: ',s%i

        call list_of_stuff%get('not there',p)  ! this one is not present in the list

        write(*,*) '--------print keys'
        call list_of_stuff%traverse_keys(print_key)

        write(*,*) '--------remove stuff3'
        call list_of_stuff%remove('stuff3')

        write(*,*) '--------print keys'
        call list_of_stuff%traverse_keys(print_key)

        write(*,*) '--------remove stuff4'
        call list_of_stuff%remove('stuff4')

        write(*,*) '--------print keys'
        call list_of_stuff%traverse_keys(print_key)

        write(*,*) '--------remove name'
        call list_of_stuff%remove('name')

        write(*,*) '--------print keys'
        call list_of_stuff%traverse_keys(print_key)

        write(*,*) '--------remove all of them'
        call list_of_stuff%remove('stuff1')
        call list_of_stuff%remove('stuff2')
        call list_of_stuff%remove('age')

        write(*,*) '--------print keys'
        call list_of_stuff%traverse_keys(print_key)

        write(*,*) '--------destroy list'
        call list_of_stuff%destroy()

        ! q was not destroyed when it was removed from the list:
        write(*,*) '--------destroy q'
        deallocate(q)

        pause
        stop

    contains

        subroutine print_key(key,done)  !! print the key for this node

        use iso_fortran_env, only: output_unit

        implicit none

        class(*),intent(in) :: key
        logical,intent(out) :: done

        character(len=10) :: key_string

        select type (key)
        type is (character(len=*))
            write(output_unit,'(A)')  'CHARACTER KEY :'// '"'//key//'"'
        type is (integer)
            write(key_string,fmt='(I10)') key
            write(output_unit,'(A)')  'INTEGER KEY   :'//trim(adjustl(key_string))
        class is (key_class)
            write(output_unit,*)      'KEY_CLASS KEY :'//'<>'
            !perhaps could use UDDTIO, but I don't want to require
            !that user always has to define this function
        end select

        done = .false.

        end subroutine print_key

    end program main
!*****************************************************************************************
