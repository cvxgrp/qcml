/*
 * TODO: MIT/BSD license this generated code
 *
 * This is ANSI C compatible template for the most generic code generation.
 * Only contains the parameter structure and the generated functions
 */

#ifndef __%(NAME)s_H__
#define __%(NAME)s_H__

#include "qcml_utils.h"

#ifdef __cplusplus
extern "C" {
#endif

/* the parameter struct */
typedef struct %(name)s_params {
%(params)s
} %(name)s_params;

/* the input dimensions struct */
typedef struct %(name)s_dims {
%(dims)s
} %(name)s_dims;

/* the solution struct
 *    users are responsible for keep track of the variable lengths
 */
typedef struct %(name)s_sol {
%(variables)s
} %(name)s_vars;
  
/* converts the '%(name)s' parameters into SOCP data
 *     allocates a qc_socp struct
 */
%(prob2socp_prototype)s;

/* assigns the pointers for the variables in '%(name)s' to point to the proper
 * memory locations in the solution vector
 */
%(socp2prob_prototype)s;

#ifdef __cplusplus
}
#endif
  
#endif // __%(NAME)s_H__
