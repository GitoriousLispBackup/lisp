project( simple )
add_library( simple SHARED simple.c )

project( uuid-gen )

add_library( uuid_gen SHARED uuid.c )
target_link_libraries( uuid_gen uuid )
