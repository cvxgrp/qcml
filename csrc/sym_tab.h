/* blah blah, license stuff
 *
 * header file for symbol table
 *
 * static memory usage
 */
   
// if you think about it carefully, there's the parser program (which is in
// the ScoopParser monad) and there's the Rewriter monad. these represent
// the two ways to think about this lex/yacc version
   
#ifndef SYMTAB_H_ // guard
#define SYMTAB_H_

#include "attributes.h"
   
struct symtab;

// what we store in the symbol table
struct scoop_val {
  sign s;
};

struct symtab *tab create_table();
void insert(struct symtab *tab, const char* key, struct scoop_val *val);
const struct scoop_val *lookup(const char* key);
void delete_table(struct symtab *tab);


#endif // SYMTAB_H_