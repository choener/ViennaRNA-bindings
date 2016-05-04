
// functions wrapped in here need a C-wrapper because they do "more work"
// before they can be called from C.

#include <stdio.h>

#include "ViennaRNA/part_func.h"
#include "ViennaRNA/part_func_co.h"
#include "ViennaRNA/duplex.h"
#include "ViennaRNA/plex.h"
#include "ViennaRNA/alifold.h"

// wrap the RNAfold constrained partition function

float ffiwrap_pf_fold_constrained (const char *sequence, char *structure, int constrained)
{
  return pf_fold_par (sequence, structure, 0, 1, constrained, 0);
}

// wrap the RNAfold constrained partition function for circular RNAs

float ffiwrap_pf_circ_fold_constrained (const char *sequence, char *structure, int constrained)
{
  return pf_fold_par (sequence, structure, 0, 1, constrained, 1);
}

// wrap the RNAcofold constrained partition function.

void ffiwrap_co_pf_fold_constrained (cofoldF * x, char *sequence, char *structure, int constrained)
{
  *x = co_pf_fold_par (sequence, structure, 0, 1, constrained);
  return;
}

// wrap RNAcofold partition function

void ffiwrap_co_pf_fold (cofoldF * x, char * inp, char * str)
{
  *x = co_pf_fold (inp, str);
  return;
}

// wrap folding a duplex

void ffiwrap_duplexfold (duplexT * x, char * linp, char * rinp)
{
  *x = duplexfold (linp, rinp);
  return;
}

