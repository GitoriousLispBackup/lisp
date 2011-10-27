project( bffi )

add_library( bffi SHARED uuid.c object.c )
target_link_libraries( bffi uuid )

project( simple )
add_library( simple SHARED simple.c )
target_link_libraries( simple bffi )


