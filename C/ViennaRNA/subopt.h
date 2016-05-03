/* subopt.h */
#ifndef VIENNA_RNA_PACKAGE_SUBOPT_H
#define VIENNA_RNA_PACKAGE_SUBOPT_H

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
 *  @addtogroup subopt_fold Enumerating Suboptimal Structures
 *  @ingroup folding_routines
 *  @{
 *    @file subopt.h
 *    @brief RNAsubopt and density of states declarations
 *
 *  @}
 */
#define VRNA_BACKWARD_COMPAT

typedef struct vrna_subopt_sol_s   vrna_subopt_solution_t;

#ifdef VRNA_BACKWARD_COMPAT

typedef struct vrna_subopt_sol_s   SOLUTION;

#endif

#include <ViennaRNA/data_structures.h>
#include <ViennaRNA/params.h>


/**
 *  @brief  Solution element from subopt.c
 */
struct vrna_subopt_sol_s {
  float energy;       /**< @brief Free Energy of structure in kcal/mol */
  char *structure;    /**< @brief Structure in dot-bracket notation */
};

/**
 *  @brief Maximum density of states discretization for subopt
 */
#define MAXDOS                1000

/**
 *  @addtogroup subopt_wuchty
 *  @{
 *
 *  @}
 */

/**
 *  @brief Returns list of subopt structures or writes to fp
 * 
 *  This function produces <b>all</b> suboptimal secondary structures within
 *  'delta' * 0.01 kcal/mol of the optimum, see @cite wuchty:1999. The results
 *  are either directly written to a 'fp' (if 'fp' is not NULL), or
 *  (fp==NULL) returned in a #vrna_subopt_solution_t * list terminated
 *  by an entry were the 'structure' member is NULL.
 *
 *  @ingroup subopt_wuchty
 *
 *  @see vrna_subopt_zuker()
 *  @param  vc
 *  @param  delta
 *  @param  sorted  Sort results by energy in ascending order
 *  @param  fp
 *  @return
 */
vrna_subopt_solution_t *
vrna_subopt(vrna_fold_compound_t *vc,
            int delta,
            int sorted,
            FILE *fp);

/**
 *  @brief Compute Zuker type suboptimal structures
 *
 *  Compute Suboptimal structures according to M. Zuker @cite zuker:1989 , i.e. for every
 *  possible base pair the minimum energy structure containing the resp. base pair.
 *  Returns a list of these structures and their energies.
 *
 *  @note This function internally uses the cofold implementation to compute
 *        the suboptimal structures. For that purpose, the function doubles
 *        the sequence and enlarges the DP matrices, which in fact will grow
 *        by a factor of 4 during the computation!
 *        At the end of the structure prediction, everything will be re-set
 *        to its original requriements, i.e. normal sequence, normal (empty)
 *        DP matrices.
 *
 *  @bug  Due to resizing, any pre-existing constraints will be lost!
 *
 *  @ingroup subopt_zuker
 *
 *  @see vrna_subopt(), zukersubopt(), zukersubopt_par()
 *
 *  @param  vc  fold compound
 *  @return     List of zuker suboptimal structures
 */
vrna_subopt_solution_t *
vrna_subopt_zuker(vrna_fold_compound_t *vc);

/**
 *  @brief printing threshold for use with logML
 * 
 *  @ingroup subopt_wuchty
 *
 */
extern  double  print_energy;

/**
 *  @brief Sort output by energy
 * 
 *  @ingroup subopt_wuchty
 *
 */
extern  int     subopt_sorted;

/**
 *  @addtogroup dos
 *  @{
 */

/**
 *  @brief The Density of States
 *
 *  This array contains the density of states for an RNA sequences after a call to subopt_par(),
 *  subopt() or subopt_circ().
 *
 *  @pre  Call one of the functions subopt_par(), subopt() or subopt_circ() prior accessing the contents
 *        of this array
 *  @see  subopt_par(), subopt(), subopt_circ()
 *
 */
extern  int     density_of_states[MAXDOS+1];

/** @} */ /* End of group dos */

#ifdef VRNA_BACKWARD_COMPAT

/**
 *  @brief Returns list of subopt structures or writes to fp
 * 
 *  This function produces <b>all</b> suboptimal secondary structures within
 *  'delta' * 0.01 kcal/mol of the optimum. The results are either
 *  directly written to a 'fp' (if 'fp' is not NULL), or
 *  (fp==NULL) returned in a #SOLUTION * list terminated
 *  by an entry were the 'structure' pointer is NULL.
 *
 *  @ingroup subopt_wuchty
 *
 *  @param  seq
 *  @param  structure
 *  @param  delta
 *  @param  fp
 *  @return
 */
DEPRECATED(SOLUTION *subopt (char *seq, char *structure, int delta, FILE *fp));

/**
 *  @brief Returns list of subopt structures or writes to fp
 * 
 *  @ingroup subopt_wuchty
 */
DEPRECATED(SOLUTION *subopt_par(char *seq, char *structure, vrna_param_t *parameters, int delta, int is_constrained, int is_circular, FILE *fp));

/**
 *  @brief Returns list of circular subopt structures or writes to fp
 * 
 *  This function is similar to subopt() but calculates secondary structures
 *  assuming the RNA sequence to be circular instead of linear
 * 
 *  @ingroup subopt_wuchty
 *
 *  @param  seq
 *  @param  sequence
 *  @param  delta
 *  @param  fp
 *  @return
 */
DEPRECATED(SOLUTION *subopt_circ(char *seq, char *sequence, int delta, FILE *fp));

/**
 *  @brief Compute Zuker type suboptimal structures
 *
 *  Compute Suboptimal structures according to M. Zuker, i.e. for every
 *  possible base pair the minimum energy structure containing the resp. base pair.
 *  Returns a list of these structures and their energies.
 *
 *  @ingroup subopt_zuker
 *
 *  @deprecated use vrna_zukersubopt() instead
 *
 *  @param  string  RNA sequence
 *  @return         List of zuker suboptimal structures
 */
DEPRECATED(SOLUTION  *zukersubopt(const char *string));

/**
 *  @brief Compute Zuker type suboptimal structures
 *
 *  @ingroup subopt_zuker
 *
 *  @deprecated use vrna_zukersubopt() instead
 *
 */
DEPRECATED(SOLUTION  *zukersubopt_par(const char *string, vrna_param_t *parameters));


#endif

#endif
