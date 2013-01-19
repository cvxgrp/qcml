%{
#include <stdio.h>
%}

DIGIT   [0-9]
CONST   [0-9]+(\.[0-9]*)?
ZERO    0+(\.0*)?
ID      [a-zA-Z][a-zA-Z0-9]*

%%
{ZERO}    printf("got ZERO!\n");
{CONST}   printf("got a (double) constant\n");

variable|parameter|minimize|maximize|find|"subject to"   printf("a keyword: %s\n", yytext);
  /* list of atoms needed here */
begin     printf("Stop command received\n");
end       printf("Start command received\n");
{ID}      printf("an identifier: %s\n", yytext);

"+"|"-"|"*"|"<="|">="|"==" printf("operator: %s\n", yytext);
"#".*     /* eat hash comment */
[ \t\n]+  /* eat whitespace */
.         printf("Unrecognized character: %s\n", yytext);
%%