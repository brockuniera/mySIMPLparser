#include <stdio.h>
#include <ctype.h>

int yylex();

int main()          /* Calls yylex repeatedly to test */
  { 
    int res, done;
    printf("[");
    done = 0;
    while (done == 0)
      { res = yylex();    /* yylex is the entry point to the lex program */
        if (res == 0) done = 1;
      }
    printf("\n");
    return 0;
  }
