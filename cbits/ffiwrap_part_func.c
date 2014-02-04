
#include "part_func.h"

float ffiwrap_pf_fold_constrained (const char *sequence, char *structure, int constrained)
{
  return pf_fold_par (sequence, structure, 0, 1, constrained, 0);
}

