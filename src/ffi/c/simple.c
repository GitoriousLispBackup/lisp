#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "ffi_object.h"

int int_sum( int a, int b )
{
  return a + b;
}

short short_sum( short a, short b )
{
  return a + b;
}

long long_sum( long a, long b )
{
  return a + b;
}

float float_sum( float a, float b )
{
  return a + b;
}

unsigned uint_sum( unsigned a, unsigned b )
{
  return a + b;
}

unsigned short ushort_sum( unsigned short a, unsigned short b )
{
  return a + b;
}

unsigned long ulong_sum( unsigned long a, unsigned long b )
{
  return a + b;
}

double double_sum( double a, double b )
{
  return a + b;
}

int bool_and( int a, int b )
{
  return a && b;
}

int sum_generated( int (*generator)() )
{
  return (*generator)() + (*generator)();
}

int double_generated( int (*generator1)(), int (*generator2)() )
{
  return sum_generated( generator1 ) + sum_generated( generator2 );
}

int ranged_generator( int (*generator)( int, int ) )
{
  int gen1 = (*generator)( 0, 10 );
  int gen2 = (*generator)( 20, 100 );

  if( gen1 <0 || gen1 > 10 )
    return 0;
  if( gen2 <20 || gen2 > 100 )
    return 0;

  return 1;
}

int simple_generator()
{
  static int value = -1;
  value = value + 1;

  return value;
}

int (*get_generator())()
{
  return &simple_generator;
}

int string_length( const char* string )
{
  return strlen( string );
}

const char* sample_string ()
{
  return "Hello, Lisp!!!";
}

typedef struct
{
  int value;
} int_holder;

const unsigned char holder_uuid [] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
const unsigned char mega_holder_uuid [] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 };

void* make_holder()
{
  int_holder* ret = malloc( sizeof( int_holder ));
  return make_object( ret, holder_uuid );
}

void delete_holder( int_holder* holder )
{
  holder->value = -1; //Let's think that -1 means that we released holder :)
}

int hld_holded( int_holder* holder )
{
  return holder->value;
}

void hld_set_holded( int_holder* holder, int value )
{
  holder->value = value;
}

void* make_mega_holder ()
{
  int_holder* ret = malloc( sizeof( int_holder ));
  return make_object( ret, mega_holder_uuid );
}
