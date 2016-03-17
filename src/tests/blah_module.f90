!*****************************************************************************************
    module blah_module
    !! An example of a derived type that can be added to the generic list.
    use linked_list_module
    
    implicit none
    
        private

        type,public :: blah
            integer :: i = 1
            contains
            final :: destroy_blah
        end type blah
        
        public :: get_blah_from_list
    
    contains
        
        subroutine destroy_blah(me)
            implicit none
            type(blah),intent(inout) :: me
            write(*,*) '...calling finalizer for blah type...'
            me%i = 0
        end subroutine destroy_blah
    
        subroutine get_blah_from_list(list_of_items,key,value)
        !! a wrapper to return a pointer to a type(blah) in a list
        
            implicit none
        
            type(list),intent(in)   :: list_of_items
            character(len=*),intent(in) :: key
            type(blah),intent(out),pointer  :: value
        
            class(*),pointer :: p
        
            call list_of_items%get(key,p) 
            if (associated(p)) then
                select type (p)
                type is (blah)
                    value => p
                end select
            else
                write(*,*) 'ERROR: '//trim(key)//' not found in the list'
                value => null()
            end if
        
        end subroutine get_blah_from_list

    end module blah_module
!*****************************************************************************************