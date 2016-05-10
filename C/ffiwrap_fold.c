
// functions wrapped in here need a C-wrapper because they do "more work"
// before they can be called from C.

#include <stdio.h>

#include "ViennaRNA/part_func.h"
#include "ViennaRNA/part_func_co.h"
#include "ViennaRNA/duplex.h"
#include "ViennaRNA/plex.h"
#include "ViennaRNA/alifold.h"
#include "ViennaRNA/fold.h"

float ffiwrap_fold_temp (float temp, const char *sequence, char *structure)
{
  float                 mfe;
  vrna_fold_compound_t  *vc;
  vrna_md_t             md;

  vrna_md_set_default(&md);
  md.temperature = temp;
  vc  = vrna_fold_compound(sequence, &md, 0);
  mfe = vrna_mfe(vc, structure);

  vrna_fold_compound_free(vc);

  return mfe;
}

