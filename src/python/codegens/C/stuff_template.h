//
// TODO: MIT/BSD license this generated code
//
// This is ANSI C compatible template for the most generic code generation.
// Only contains the parameter structure and the generated functions
//

#ifndef __%(name)s__H__
#define __%(name)s__H__

#include "qcml_utils.h"

// the parameter struct
typedef struct params {
%(params)s
} %(name)s_params;

typedef struct sol {
%(variables)s
} %(name)s_vars;

%(prob2socp_prototype)s;
%(socp2prob_prototype)s;
  
#endif // __%(name)s__H__