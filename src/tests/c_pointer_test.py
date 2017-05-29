
""" Some experiments with python ctypes """

#  build the shared library using something like:
#
# ifort -shared -fPIC key_module.f90 linked_list_module.f90 tests/blah_module.f90 tests/c_pointer_test_module.f90 -o test.so

from ctypes import *

# load the shared library:
test = CDLL('test.so')

# define the procedure interfaces:
initialize_list = test.initialize_list
create_model    = test.create_model
access_model    = test.access_model
destroy_model   = test.destroy_model
destroy_list    = test.destroy_list
initialize_list.restype = None
access_model.restype    = None
destroy_model.restype   = None
destroy_list.restype    = None
create_model.argtypes   = [c_int]
create_model.restype    = POINTER(c_int)  # `c_long` also seems to work ?
                                          # not really sure what is correct for this.
                                          # Fortran returns a `type(c_ptr)`, maybe
                                          # in this context it doesn't matter

print('')
print( 'calling initialize_list...')

initialize_list()

print( 'calling create_model...')

i = c_int(989)   # some input to the model
cp = create_model(i)

#cp = byref(cp)  # - don't need this if subroutine argument has `value` attribute

print( 'calling access_model...')

for j in range(10):
    access_model(cp)

print( 'calling destroy_model...')

destroy_model(cp)

print( 'calling destroy_list...')

destroy_list()
