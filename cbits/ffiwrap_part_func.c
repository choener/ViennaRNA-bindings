
// functions wrapped in here need a C-wrapper because they do "more work"
// before they can be called from C.

#include <stdio.h>

#include "part_func.h"
#include "part_func_co.h"

// wrap the RNAfold constrained partition function

float ffiwrap_pf_fold_constrained (const char *sequence, char *structure, int constrained)
{
  return pf_fold_par (sequence, structure, 0, 1, constrained, 0);
}

// wrap the RNAcofold constrained partition function.
//
// TODO the current implementation makes me cry.

cofoldF * ffiwrap_co_pf_fold_constrained (char *sequence, char *structure, int constrained)
{
//  printf ("%p\n",sequence);
//  printf ("%p\n",structure);
//  fflush (stdout);
//  printf ("%s\n",sequence);
//  printf ("%s\n",structure);
//  fflush (stdout);
  cofoldF x = co_pf_fold_par (sequence, structure, 0, 1, constrained);
//  printf ("%s\n",structure);
  return &x;
}

// wrap RNAcofold partition function

cofoldF * ffiwrap_co_pf_fold (char * inp, char * str)
{ 
//  printf ("%p\n",inp);
//  printf ("%p\n",str);
//  fflush (stdout);
//  printf ("%s\n",inp);
//  printf ("%s\n",str);
//  fflush (stdout);
  cofoldF x = co_pf_fold (inp, str);
  return &x;
}

