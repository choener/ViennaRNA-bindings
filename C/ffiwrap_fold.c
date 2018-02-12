
// functions wrapped in here need a C-wrapper because they do "more work"
// before they can be called from C.

#include <stdio.h>

#include "ViennaRNA/part_func.h"
#include "ViennaRNA/part_func_co.h"
#include "ViennaRNA/duplex.h"
#include "ViennaRNA/plex.h"
#include "ViennaRNA/alifold.h"
#include "ViennaRNA/fold.h"

float ffiwrap_fold_temp (int nolp, float temp, const char *sequence, char *structure)
{
  float                 mfe;
  vrna_fold_compound_t  *vc;
  vrna_md_t             md;

  vrna_md_set_default(&md);
  md.noLP = nolp;
  md.temperature = temp;
  vc  = vrna_fold_compound(sequence, &md, 0);
  mfe = vrna_mfe(vc, structure);

  vrna_fold_compound_free(vc);

  return mfe;
}

float
ffiwrap_eos_temp (float temp, const char *string, const char *structure)
{
  float e;

  vrna_md_t             md;
  vrna_md_set_default(&md);
  md.temperature = temp;

  /* create fold_compound with default parameters and without DP matrices */
  vrna_fold_compound_t *vc = vrna_fold_compound(string, &md, VRNA_OPTION_EVAL_ONLY);

  /* evaluate structure */
  e = vrna_eval_structure(vc, structure);

  /* free fold_compound */
  vrna_fold_compound_free(vc);

  return e;
}


