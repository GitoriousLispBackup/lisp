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

int sum_generated ( int (*generator)() )
{
  return (*generator)() + (*generator)();
}
