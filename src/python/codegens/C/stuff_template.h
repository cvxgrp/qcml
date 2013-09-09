//
// TODO: MIT/BSD license this generated code
//
// This is ANSI C compatible template for the most generic code generation.
// Only contains the parameter structure and the generated functions
//

#ifndef __%(NAME)s_H__
#define __%(NAME)s_H__

#include "qcml_utils.h"

// the parameter struct
typedef struct params {
%(params)s
} %(name)s_params;

// the solution struct
//    users are responsible for keep track of the variable lengths
typedef struct sol {
%(variables)s
} %(name)s_vars;

// converts the '%(name)s' parameters into SOCP data
//     allocates a qc_socp struct
%(prob2socp_prototype)s;

// assigns the pointers for the variables in '%(name)s' to point to the proper
// memory locations in the solution vector
%(socp2prob_prototype)s;
  
#endif // __%(NAME)s_H__