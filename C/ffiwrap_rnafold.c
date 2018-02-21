
/*
 * Generic RNAfold wrapper. Depending on the options given, calculates mfe,
 * centroid, ensemble results.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ViennaRNA/part_func.h"
#include "ViennaRNA/part_func_co.h"
#include "ViennaRNA/duplex.h"
#include "ViennaRNA/plex.h"
#include "ViennaRNA/alifold.h"
#include "ViennaRNA/fold.h"

int ffiwrap_RNAfold_init
  // options
  ( int withmfe       // enable mfe calcualtion
  , int withpartfun   // enable partition function calculation
  , int withcentroid  // enable centroid calculation
  , int nolp
  , int dangles
  , int noguclosure
  // temperature
  , float temp
  // input
  , const char *s1
  // mfe results
  , char *s2mfe
  , float *emfe
  // centroid
  , char *s2centroid
  , double *ecentroid
  , double *centroiddistance
  )
{
  float epf = 0;
  char *s2partfun = 0;
  char *s2tmpcentroid = 0;

  vrna_fold_compound_t  *vc;
  vrna_md_t             md;

  vrna_md_set_default(&md);
  md.noLP = nolp;
  md.dangles = dangles;
  md.noGUclosure = noguclosure;
  md.temperature = temp;
  vc  = vrna_fold_compound(s1, &md, 0);

  // calculate mfe
  if (withmfe) {
    *emfe = vrna_mfe(vc, s2mfe);
  } else {
    *emfe = 0;
    s2mfe[0] = 0;
  }
  // partition function
  if (withpartfun) {
    epf = vrna_pf(vc, s2partfun);
  } else {
    epf = 0;
    s2partfun = 0;
  }
  // centroid
  if (withcentroid) {
    s2tmpcentroid = vrna_centroid(vc, centroiddistance);
    *ecentroid = vrna_eval_structure(vc, (const char*) s2tmpcentroid);
    strcpy (s2centroid, s2tmpcentroid);
    free (s2tmpcentroid);
    free (s2partfun);
  } else{
    *ecentroid = 0;
    s2tmpcentroid = 0;
  }

  vrna_fold_compound_free(vc);

  return 0;
}

