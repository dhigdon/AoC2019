// Day 2, part B
// C version

#include <stdio.h>

int calc(int mass)
{
   int fuel = 0;
   while (mass > 0)
   {
      // Fuel is mass/3 - 2
      mass = (mass/3) - 2;
      fuel += mass;
   }
   return fuel;
}

int main(int argc, char **argv)
{
   if (argc > 1)
   {
      int mass, fuel, total;
      FILE * f = fopen(argv[1], "r");

      total = 0;
      while (!feof( f ))
      {
         fscanf( f, "%d\n", &mass);
         fuel = calc( mass );
         total += fuel;
         printf( "Mass = %d, Fuel = %d\n", mass, fuel );
      }
      fclose( f );

      printf( "Total mass = %d\n", total );

      return 0;
   }
   return 1;
}
