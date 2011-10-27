#include <stdlib.h>
#include <string.h>
#include "ffi_object.h"

ffi_object make_object( void* value, const unsigned char type[] )
{
  ffi_object object = malloc( sizeof( ffi_object_base ));
  object->value = value;
  object->type = malloc( 16 * sizeof( unsigned char ));
  memcpy( object->type, type, 16 * sizeof( unsigned char ));

  return object;
}
