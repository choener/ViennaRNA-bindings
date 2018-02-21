
/*
 * Generic RNAfold wrapper. Depending on the options given, calculates mfe,
 * centroid, ensemble results.
 *
 * TODO Needs the option to hand in different energy parameters.
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

int ffiwrap_RNAfold
  // input options
  ( int withmfe       // enable mfe calcualtion
  , int withpartfun   // enable partition function calculation
  , int withcentroid  // enable centroid calculation
  , int nolp
  , int dangles
  , int noguclosure
  // temperature
  , float temp
  // the actual input sequence
  , const char *s1
  // output options
  // mfe results
  , char *s2mfe
  , float *emfe
  // partition function string
  , char *s2ensemble  // this string is *not* a canonical secondary structure string
  , float *eensemble  // energy of the whole ensemble
  // centroid
  , char *s2centroid
  , double *ecentroid
  , double *centroiddistance
  )
{
  vrna_fold_compound_t  *vc;
  vrna_md_t             md;

  vrna_md_set_default(&md);
  // TODO if ptr to model energy is non-zero then use different energy model
  md.noLP = nolp;
  md.dangles = dangles;
  md.noGUclosure = noguclosure;
  md.temperature = temp;

  if (s1==NULL || strlen (s1) == 0)
  {
    // no mfe
    *emfe = 0;
    s2mfe[0] = 0;
    // no ensemble
    *eensemble = 0;
    s2ensemble[0] = 0;
    // no centroid
    *ecentroid = 0;
    *centroiddistance = 0;
    s2centroid[0] = 0;
    return 0;
  }

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
    *eensemble = vrna_pf(vc, s2ensemble);
  } else {
    *eensemble = 0;
    s2ensemble[0] = 0;
  }
  // centroid
  if (withcentroid) {
    s2centroid = vrna_centroid(vc, centroiddistance);
    *ecentroid = vrna_eval_structure(vc, (const char*) s2centroid);
  } else{
    *ecentroid = 0;
    *centroiddistance = 0;
    s2centroid[0] = 0;
  }

  vrna_fold_compound_free(vc);

  return 0;
}

