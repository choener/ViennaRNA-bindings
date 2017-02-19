
// functions wrapped in here need a C-wrapper because they do "more work"
// before they can be called from C.

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ViennaRNA/part_func.h"
#include "ViennaRNA/part_func_co.h"
#include "ViennaRNA/duplex.h"
#include "ViennaRNA/plex.h"
#include "ViennaRNA/alifold.h"
#include "ViennaRNA/fold.h"

double ffiwrap_centroid_temp (double temp, const char *sequence, char *structure)
{
  double                dist, e;
  char*                 cent;
  char*                 pf_struc;
  double                cent_en;
  vrna_fold_compound_t  *vc;
  vrna_md_t             md;

  vrna_md_set_default(&md);
  md.noLP = 1;
  md.temperature = temp;
  vc  = vrna_fold_compound(sequence, &md, 0);
  e = vrna_pf(vc, pf_struc);
  cent = vrna_centroid(vc, &dist);
  cent_en = vrna_eval_structure(vc, (const char*) cent);
  strcpy (structure, cent);
  free (cent);
  free (pf_struc);

  vrna_fold_compound_free(vc);

  return cent_en;
}

