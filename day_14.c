// C version of Day 14, for benchmarking purposes
// by Dan Higdon

#include <stdio.h>
#include <string.h>

//------------------------------------------------------------------------ 

typedef struct __Rule
{
   int reactant;
   int quantity;
} Rule;



//------------------------------------------------------------------------ 

typedef struct __Crucbile
{
   char**   reactants;     // 
   int*     quantities;    // 
   int*     totals;        // 
} Crucible;

void init_crucible( Crucible * p, int length, char** names)
{
   p->quantities = malloc(sizeof(int) * length);
   memset(p->quantities, 0, sizeof(int) * length);

   p->totals = malloc(sizeof(int) * length);
   memset(p->totals, 0, sizeof(int) * length);

   p->reactants = names; // TODO?
}

void free_crucible(Crucible * p)
{
   free(p->quantities);
   free(p->totals);
   p=>quantities = NULL;
   p->totals = NULL;
   p->reactants = NULL;
}

int reactant_index(Crucible * p, char * name)
{
   int result = 0;
   for (char * p = p->reactants; *p; ++p, ++result)
   {
      if (!stricmp(name, p))
         return result;
   }
   return -1;
}

int total(Crucible * p, int reactant)
{
   return p->totals[reactant];
}

int quantity(Crucible * p, int reactant)
{
   return p->quantities[reactant];
}

int consumed(Crucible * p, int reactant)
{
   return p->totals[reactant] - p->quantities[reactant];
}

int set_contents(Crucible * p, int reactant, int value)
{
   p->totals[reactant] = p->quantities[reactant] = value;
}

int add(Crucible* p, int reactant, int value)
{
   p->totals[reactant] += value;
   p->quantities[reactant] += value;
   return p->quantities[reactant];
}

int consume(Crucible *p, int reactant, int value)
{
   int new = p->quantities[reactant] - value;
   if (new >= 0)
   {
      p->quantities[reactant] = new;
      return new;
   }
   return -1;
}

//------------------------------------------------------------------------ 

int produce( int goal, int required, Rules * rules, Crucible * p)
{
   if ( quantity(p, goal) >= required )
   {
      return required;
   }
   else if ( goal == 0 )
   {
      return add(p, goal, required - quantity(p, goal));
   }
   else
   {
      // Find rule

      int produced = 0;
      while (produced < required)
      {
         // break the rule apart
         int produced_quantity = TODO;

         for (int n = 0; n < nrules; ++n)
         {
            int reactant = TODO;
            int quantity = TODO;

            // run rules
            if (produce( reactant, quantity, rules, q ) != -1)
            {
               consume(p, reactant, quantity);
               produced += produced_quantity;
            }
            else
            {
               return -1;
            }
         }
      }
      return add(p, goal, produced);
   }
}

//------------------------------------------------------------------------ 
//------------------------------------------------------------------------ 

