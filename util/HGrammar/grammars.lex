%{
#include "grammars.tab.h"

void saveWord();
%}

letter   [a-zA-Z]
digit    [0-9]
white    [ \t\n]+

/* Comment states */
%x COMMENT
%x MULTICOMMENT

%%

"/*"                           { BEGIN(MULTICOMMENT); }
<MULTICOMMENT>"*/"             { BEGIN(INITIAL); }

"//"                           { BEGIN(COMMENT); }
<COMMENT>"\n"                  { BEGIN(INITIAL); }

<COMMENT>.                     { /* Ignore, because we are within a comment. */ }
<MULTICOMMENT>.                { /* Ignore, because we are within a comment. */ }

{letter}({letter}|{digit})*    { saveWord(); return T_WORD; }
"%"tokens                      { return T_DIR_TOKEN; }
"%"token                       { return T_DIR_TOKEN; }
"%"start                       { return T_DIR_START; }
":"                            { return T_COLON; }
";"                            { return T_SEMICOLON; }
"|"                            { return T_PIPE; }
"%""%"                         { return T_SEPERATOR; }
{white}                        { /* ignore it all */ }
.                              { printf("Unexpected symbol: %c\n", yytext[0]); }

%%

void saveWord() {
   yylval.word = malloc(strlen(yytext)+1);
   strcpy(yylval.word, yytext);
}

void initLexer(char* filename) {
   FILE *f = fopen(filename, "r");
   if (f == NULL) {
      fprintf(stderr, "\nError: \tfailed to open file \"%s\"\n\n", filename);
      exit(EXIT_FAILURE);
   }

   /* Set lexer input */
   yyin = f;
}

void finalizeLexer() {
  fclose(yyin);
  yylex_destroy();
}
