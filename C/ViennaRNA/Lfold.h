#ifndef VIENNA_RNA_PACKAGE_LFOLD_H
#define VIENNA_RNA_PACKAGE_LFOLD_H

/**
 */

/* make this interface backward compatible with RNAlib < 2.2.0 */
#define VRNA_BACKWARD_COMPAT

#ifdef DEPRECATION_WARNINGS
# ifdef __GNUC__
#  define DEPRECATED(func) func __attribute__ ((deprecated))
# else
#  define DEPRECATED(func) func
# endif
#else
# define DEPRECATED(func) func
#endif

/**
 *  @addtogroup local_fold
 *
 *  Local structures can be predicted by a modified version of the
 *  fold() algorithm that restricts the span of all base pairs.
 *  @{
 *    @file Lfold.h
 *    @brief Predicting local MFE structures of large sequences
 *
 *  @}
 */

#include <ViennaRNA/mfe.h>

/**
 *  @addtogroup local_mfe_fold
 *  @{
 *
 *  @}
 */

/**
 *  @brief Local MFE prediction using a sliding window approach (simplified interface)
 * 
 *  This simplified interface to vrna_mfe_window() computes the MFE and locally
 *  optimal secondary structure using default options. Structures are predicted
 *  using a sliding window approach, where base pairs may not span outside the
 *  window. Memory required for dynamic programming (DP) matrices will
 *  be allocated and free'd on-the-fly. Hence, after return of this function, the recursively filled
 *  matrices are not available any more for any post-processing.
 *
 *  @note In case you want to use the filled DP matrices for any subsequent post-processing step, or
 *  you require other conditions than specified by the default model details, use vrna_mfe_window(),
 *  and the data structure #vrna_fold_compound_t instead.
 *
 *  @ingroup local_mfe_fold
 *
 *  @see  vrna_mfe_window(), vrna_Lfoldz(), vrna_mfe_window_zscore(), vrna_fold_compound(),
 *        #vrna_fold_compound_t
 *
 *  @param  string      The nucleic acid sequence
 *  @param  window_size The window size for locally optimal structures
 *  @param  file        The output file handle where predictions are written to (if NULL, output is written to stdout)
 */
float
vrna_Lfold( const char *string,
            int window_size,
            FILE  *file);

#ifdef USE_SVM
/**
 *  @brief Local MFE prediction using a sliding window approach with z-score cut-off (simplified interface)
 * 
 *  This simplified interface to vrna_mfe_window_zscore() computes the MFE and locally
 *  optimal secondary structure using default options. Structures are predicted
 *  using a sliding window approach, where base pairs may not span outside the
 *  window. Memory required for dynamic programming (DP) matrices will
 *  be allocated and free'd on-the-fly. Hence, after return of this function, the recursively filled
 *  matrices are not available any more for any post-processing.
 *  This function is the z-score version of vrna_Lfold(), i.e.
 *  only predictions above a certain z-score cut-off value are
 *  printed.
 *
 *  @note In case you want to use the filled DP matrices for any subsequent post-processing step, or
 *  you require other conditions than specified by the default model details, use vrna_mfe_window(),
 *  and the data structure #vrna_fold_compound_t instead.
 *
 *  @ingroup local_mfe_fold
 *
 *  @see  vrna_mfe_window_zscore(), vrna_Lfold(), vrna_mfe_window(), vrna_fold_compound(),
 *        #vrna_fold_compound_t
 *
 *  @param  string      The nucleic acid sequence
 *  @param  window_size The window size for locally optimal structures
 *  @param  min_z       The minimal z-score for a predicted structure to appear in the output
 *  @param  file        The output file handle where predictions are written to (if NULL, output is written to stdout)
 */
float
vrna_Lfoldz(const char *string,
            int window_size,
            double min_z,
            FILE *file);

#endif


/**
 *  @addtogroup local_consensus_fold
 *  @{
 *
 *  @}
 */

/**
 *  @brief
 *
 *  @ingroup local_consensus_fold
 * 
 *  @param strings
 *  @param structure
 *  @param maxdist
 *  @return
 */
float aliLfold( const char **strings,
                char *structure,
                int maxdist);

#ifdef  VRNA_BACKWARD_COMPAT

/**
 *  @brief The local analog to fold().
 * 
 *  Computes the minimum free energy structure including only base pairs
 *  with a span smaller than 'maxdist'
 *
 *  @ingroup local_mfe_fold
 *
 *  @deprecated Use vrna_mfe_window() instead!
 */
DEPRECATED(float Lfold(const char *string, char *structure, int maxdist));

/**
 *  @brief
 * 
 *  @ingroup local_mfe_fold
 * 
 *  @deprecated Use vrna_mfe_window_zscore() instead!
 */
DEPRECATED(float Lfoldz(const char *string, char *structure, int maxdist, int zsc, double min_z));

#endif

#endif
