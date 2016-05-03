#ifndef VIENNA_RNA_PACKAGE_COFOLD_H
#define VIENNA_RNA_PACKAGE_COFOLD_H

#include <ViennaRNA/data_structures.h>
#include <ViennaRNA/params.h>
#include <ViennaRNA/mfe.h>

#ifdef DEPRECATION_WARNINGS
# ifdef __GNUC__
#  define DEPRECATED(func) func __attribute__ ((deprecated))
# else
#  define DEPRECATED(func) func
# endif
#else
# define DEPRECATED(func) func
#endif

/* make this interface backward compatible with RNAlib < 2.2.0 */
#define VRNA_BACKWARD_COMPAT

/**
 *  @addtogroup cofold
 *  @brief Predict structures formed by two molecules upon hybridization.
 *
 *  The function of an RNA molecule often depends on its interaction with
 *  other RNAs. The following routines therefore allow to predict structures
 *  formed by two RNA molecules upon hybridization.\n
 *  One approach to co-folding two RNAs consists of concatenating the two
 *  sequences and keeping track of the concatenation point in all energy
 *  evaluations. Correspondingly, many of the cofold() and
 *  co_pf_fold() routines below take one sequence string as argument
 *  and use the the global variable #cut_point to mark the concatenation
 *  point. Note that while the <i>RNAcofold</i> program uses the '&' character
 *  to mark the chain break in its input, you should not use an '&' when using
 *  the library routines (set #cut_point instead).\n
 *  In a second approach to co-folding two RNAs, cofolding is seen as a
 *  stepwise process. In the first step the probability of an unpaired region
 *  is calculated and in a second step this probability of an unpaired region
 *  is  multiplied with the probability of an interaction between the two RNAs.
 *  This approach is implemented for the interaction between a long
 *  target sequence and a short ligand RNA. Function pf_unstru() calculates
 *  the partition function over all unpaired regions in the input
 *  sequence. Function pf_interact(), which calculates the
 *  partition function over all possible interactions between two
 *  sequences, needs both sequence as separate strings as input.
 *
 */

/**
 *  @addtogroup mfe_cofold
 *  @{
 *  @file cofold.h
 *
 *  @brief MFE version of cofolding routines
 *
 *  This file includes (almost) all function declarations within the <b>RNAlib</b> that are related to
 *  MFE Cofolding...
 *  This also includes the Zuker suboptimals calculations, since they are implemented using the cofold
 *  routines.
 */

/**
 *  @brief Compute Minimum Free Energy (MFE), and a corresponding secondary structure for two dimerized RNA sequences
 *
 *  This simplified interface to vrna_mfe() computes the MFE and, if required, a secondary structure for
 *  two RNA sequences upon dimerization using default options. Memory required for dynamic programming
 *  (DP) matrices will be allocated and free'd on-the-fly. Hence, after return of this function, the
 *  recursively filled matrices are not available any more for any post-processing, e.g. suboptimal
 *  backtracking, etc.
 *
 *  @note In case you want to use the filled DP matrices for any subsequent post-processing step, or
 *  you require other conditions than specified by the default model details, use vrna_mfe(),
 *  and the data structure #vrna_fold_compound_t instead.
 *
 *  @see vrna_mfe_dimer(), vrna_fold_compound(), #vrna_fold_compound_t, vrna_cut_point_insert()
 *
 *  @param sequence   two RNA sequences separated by the '&' character
 *  @param structure  A pointer to the character array where the
 *         secondary structure in dot-bracket notation will be written to
 *  @return the minimum free energy (MFE) in kcal/mol
 */
float
vrna_cofold(const char *string,
            char *structure);

#ifdef VRNA_BACKWARD_COMPAT

/**
 *  @brief Compute the minimum free energy of two interacting RNA molecules
 *
 *  The code is analog to the fold() function. If #cut_point ==-1 results
 *  should be the same as with fold().
 *
 *  @ingroup mfe_cofold
 *
 *  @deprecated use vrna_mfe_dimer() instead
 *
 *  @param    sequence  The two sequences concatenated
 *  @param    structure Will hold the barcket dot structure of the dimer molecule
 *  @return   minimum free energy of the structure
 */
DEPRECATED(float
cofold( const char *sequence,
        char *structure));

/**
 *  @brief Compute the minimum free energy of two interacting RNA molecules
 *
 *  @deprecated use vrna_mfe_dimer() instead
 *
 */
DEPRECATED(float
cofold_par( const char *string,
            char *structure,
            vrna_param_t *parameters,
            int is_constrained));

/**
 *  @brief Free memory occupied by cofold()
 *
 *  @deprecated This function will only free memory allocated by a prior call of cofold() or cofold_par().
 *  See vrna_mfe_dimer() for how to use the new API
 *
 *  @note folding matrices now reside in the fold compound, and should be free'd there
 *  @see  vrna_fc_destroy(), vrna_mfe_dimer()
 */
DEPRECATED(void free_co_arrays(void));

/**
 *  @brief Recalculate parameters
 *  @deprecated See vrna_params_subst() for an alternative using the new API
 */
DEPRECATED(void update_cofold_params(void));

/**
 *  @brief Recalculate parameters
 *  @deprecated See vrna_params_subst() for an alternative using the new API
 */
DEPRECATED(void update_cofold_params_par(vrna_param_t *parameters));


/**
 *  @brief Export the arrays of partition function cofold (with gquadruplex support)
 *
 *  Export the cofold arrays for use e.g. in the concentration
 *  Computations or suboptimal secondary structure backtracking
 *
 *  @deprecated folding matrices now reside within the fold compound. Thus, this function will
 *  only work in conjunction with a prior call to cofold() or cofold_par()
 *
 *  @see vrna_mfe_dimer() for the new API
 *
 *  @param  f5_p    A pointer to the 'f5' array, i.e. array conatining best free energy in interval [1,j]
 *  @param  c_p     A pointer to the 'c' array, i.e. array containing best free energy in interval [i,j] given that i pairs with j
 *  @param  fML_p   A pointer to the 'M' array, i.e. array containing best free energy in interval [i,j] for any multiloop segment with at least one stem
 *  @param  fM1_p   A pointer to the 'M1' array, i.e. array containing best free energy in interval [i,j] for multiloop segment with exactly one stem
 *  @param  fc_p    A pointer to the 'fc' array, i.e. array ...
 *  @param  ggg_p   A pointer to the 'ggg' array, i.e. array containing best free energy of a gquadruplex delimited by [i,j]
 *  @param  indx_p  A pointer to the indexing array used for accessing the energy matrices
 *  @param  ptype_p A pointer to the ptype array containing the base pair types for each possibility (i,j)
 */
DEPRECATED(void export_cofold_arrays_gq(int **f5_p,
                                        int **c_p,
                                        int **fML_p,
                                        int **fM1_p,
                                        int **fc_p,
                                        int **ggg_p,
                                        int **indx_p,
                                        char **ptype_p));

/**
 *  @brief Export the arrays of partition function cofold
 *
 *  Export the cofold arrays for use e.g. in the concentration
 *  Computations or suboptimal secondary structure backtracking
 *
 *  @deprecated folding matrices now reside within the #vrna_fold_compound_t. Thus, this function will
 *  only work in conjunction with a prior call to the deprecated functions cofold() or cofold_par()
 *
 *  @see vrna_mfe_dimer() for the new API
 *
 *  @param  f5_p    A pointer to the 'f5' array, i.e. array conatining best free energy in interval [1,j]
 *  @param  c_p     A pointer to the 'c' array, i.e. array containing best free energy in interval [i,j] given that i pairs with j
 *  @param  fML_p   A pointer to the 'M' array, i.e. array containing best free energy in interval [i,j] for any multiloop segment with at least one stem
 *  @param  fM1_p   A pointer to the 'M1' array, i.e. array containing best free energy in interval [i,j] for multiloop segment with exactly one stem
 *  @param  fc_p    A pointer to the 'fc' array, i.e. array ...
 *  @param  indx_p  A pointer to the indexing array used for accessing the energy matrices
 *  @param  ptype_p A pointer to the ptype array containing the base pair types for each possibility (i,j)
 */
DEPRECATED(void export_cofold_arrays( int **f5_p,
                                      int **c_p,
                                      int **fML_p,
                                      int **fM1_p,
                                      int **fc_p,
                                      int **indx_p,
                                      char **ptype_p));



/**
 *  @brief get_monomer_free_energies
 *
 *  Export monomer free energies out of cofold arrays
 *  @deprecated{This function is obsolete and will be removed soon!}
 *
 *  @param e1 A pointer to a variable where the energy of molecule A will be written to
 *  @param e2 A pointer to a variable where the energy of molecule B will be written to
 */
DEPRECATED(void get_monomere_mfes( float *e1, float *e2));


/**
 *  allocate arrays for folding
 *  @deprecated{This function is obsolete and will be removed soon!}
 */
DEPRECATED(void initialize_cofold(int length));

#endif

/**
 *  @}
 */


#endif
