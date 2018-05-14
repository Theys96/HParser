%{
#include <stdio.h>
#include <stdlib.h>
#include "stringList.h"
#include "lex.yy.c"

int yyerror(char *s) {
  finalizeLexer();
  printf("Parse error.\n");
  exit(-1);
}

typedef enum {
 Terminal, NonTerminal
} TokenType;

char* moduleName;
char* grammarName;
char* outFile;
FILE* out;

char* startSymbol;
StringList terminals;
void setStartSymbol(char* sym);
void setTerminals(StringList tokens);
TokenType wordType(char* word);

void initParser(char* inputName);
void finalizeParser();

void writeGrammarStart();
void writeRules(char* lhs, StringListList lhss);
void writeGrammarEnd();

int noRules = 1;

%}

%token T_WORD T_DIR_TOKEN T_DIR_START T_SEMICOLON T_PIPE T_COLON T_SEPERATOR
%start Grammar

%union {
  char* word;
  StringList wordList;
  StringListList wordListList;
}

%type<word> T_WORD
%type<wordList> TokenList
%type<wordListList> RuleRightHands

%%

Grammar : Directives T_SEPERATOR { writeGrammarStart(); } RuleList { writeGrammarEnd(); }
        ;

Directives : TokenDirective StartDirective
           | StartDirective TokenDirective
           ;

TokenDirective : T_DIR_TOKEN TokenList { setTerminals($2); } ;

StartDirective : T_DIR_START T_WORD { setStartSymbol($2); free($2); };

RuleList : RuleList Rule
         | 
         ;

Rule : T_WORD T_COLON RuleRightHands T_SEMICOLON  { writeRules($1, $3); }

RuleRightHands : RuleRightHands T_PIPE TokenList  { consumeStringList(&$1, &$3);
                                                    $$ = $1; }
               | TokenList                        { StringListList list = newStringListList();
                                                    consumeStringList(&list, &$1);
                                                    $$ = list; }
               ;

TokenList : TokenList T_WORD      { insertStringList(&$1, $2);
                                    $$ = $1; }
          |                       { $$ = newStringList(); }
          ;

%%

int main(int argc, char *argv[]) {
  if (argc < 3) {
    printf("\nUsage: \t%s <file> <name>\n\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  initParser(argv[2]);
  initLexer(argv[1]);
  yyparse();
  finalizeLexer();
  finalizeParser();
  printf("Success.\n");
  return 0;
}

void writeGrammarStart() {
  fprintf(out, "module %s (%s) where\n\n", moduleName, grammarName);
  fprintf(out, "import HParser.Grammar\n");
  fprintf(out, "import HParser.Generator\n\n");
  fprintf(out, "%s = Grammar (NonTerminal \"%s\") [\n", grammarName, startSymbol);
}

void writeRules(char* lhs, StringListList rhs) {
  char* token;
  int s;
  for (int i = 0; i < rhs.size; i++) {
    if (!noRules) {
      fprintf(out, ",\n");
    }
    fprintf(out, "   Rule (NonTerminal \"%s\") [", lhs);
    s = 0;
    while (rhs.list[i][s] != NULL) {
      if (s != 0) {
        fprintf(out, ", ");
      }
      token = rhs.list[i][s];
      if (wordType(token) == Terminal) {
        fprintf(out, "Terminal \"%s\"", token);
      } else if (wordType(token) == NonTerminal) {
        fprintf(out, "NonTerminal \"%s\"", token);
      }
      s++;
    }
    fprintf(out, "]");
    noRules = 0;
  }
}

void writeGrammarEnd() {
  fprintf(out, "\n   ]\n");
}

void initParser(char* inputName) {
  moduleName = malloc((strlen(inputName)+1)*sizeof(char));
  grammarName = malloc((strlen(inputName)+1)*sizeof(char));
  strcpy(moduleName, inputName);
  strcpy(grammarName, inputName);
  if (moduleName[0] >= 'a') {
    moduleName[0] -= 'a'-'A';
  }
  if (grammarName[0] <= 'Z') {
    grammarName[0] += 'a'-'A';
  }
  outFile = calloc(strlen(moduleName)+4,1);
  sprintf(outFile, "%s.hs", moduleName);
  out = fopen(outFile, "wb");
  free(outFile);
}

void finalizeParser() {
  free(startSymbol);
  freeStringList(&terminals);
  free(moduleName);
  free(grammarName);
  fclose(out);
}

void setStartSymbol(char* sym) {
  startSymbol = malloc(strlen(sym)+1);
  strcpy(startSymbol, sym);
}

void setTerminals(StringList tokens) {
  terminals = tokens;
}

TokenType wordType(char* word) {
  for (int i = 0; i < terminals.size; i++) {
    if (strcmp(terminals.list[i], word) == 0) {
      return Terminal;
    }
  }
  return NonTerminal;
}
