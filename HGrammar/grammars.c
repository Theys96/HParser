#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
#include <ctype.h> 

char* outFile;
char* inFile;
FILE* in;
FILE* out;

#define TRUE 1
#define FALSE 0

void abort() {
   remove(outFile);
   fclose(in);
   fclose(out);
   exit(-1);
}

int isSkipped(char c) {
   return (
      c == ' ' || c == '\n' || c == '\t' || c == '\r' 
   );
}

void skipAll() {
   char c = fgetc(in);
   while (isSkipped(c)) {
      c = fgetc(in);
   }
   ungetc(c, in);
}

void readChar(char c) {
   skipAll();
   char read = fgetc(in);
   if (c != read) {
      printf("Error on '%c'.\n", read);
      abort();
   }
}

char nextChar() {
   skipAll();
   char c = fgetc(in);
   ungetc(c, in);
   return c;
}

char* readNonTerminal() {
   skipAll();
   char* buffer = calloc(100,1);
   char c = fgetc(in);
   if (!isalpha(c)) {
      printf("Error on '%c', non-terminals must start with a letter.\n", c);
      free(buffer);
      abort();
   }
   int i = 0;
   buffer[i++] = c;
   c = fgetc(in);
   while (isalnum(c)) {
      buffer[i++] = c;
      c = fgetc(in);
   }
   ungetc(c, in);
   fprintf(out, "(NonTerminal \"%s\")", buffer);
   return buffer;
}

char* readTerminal() {
   char* buffer = calloc(2,1);
   readChar('\'');
   buffer[0] = fgetc(in);
   readChar('\'');
   fprintf(out, "(Terminal \"%s\")", buffer);
   return buffer;
}

int readSymbol(int comma) {
   skipAll();
   char c = fgetc(in);
   if (c == '\'') {
      ungetc(c, in);
      if (comma) fprintf(out, ", ");
      free(readTerminal());
      return TRUE;
   } else if (isalpha(c)) {
      ungetc(c, in);
      if (comma) fprintf(out, ", ");
      free(readNonTerminal());
      return TRUE;
   } else {
      ungetc(c, in);
      return FALSE;
   }
}

char* readRule(char* nt0) {
   fprintf(out, "   Rule ");
   char* nt;
   if (nt0 == NULL) {
      nt = readNonTerminal();
      readChar(':');
   } else {
      fprintf(out, "(NonTerminal \"%s\")", nt0);
   }
   fprintf(out, " [");
   readSymbol(FALSE);
   while (readSymbol(TRUE)) {}
   fprintf(out, "]");
   return nt;
}

int main(int argc, char** argv) {
   if (argc < 2) {
      printf("\n\tUsage: %s grammarfile\n\n", argv[0]);
      exit(-1);
   }

   inFile = argv[1];
   in = fopen(inFile, "r");
   if (!in) {
      printf("Could not open file '%s'\n", inFile);
      exit(-1);
   }

   outFile = calloc(strlen(argv[1])+4,1);
   sprintf(outFile, "%s.hs", argv[1]);
   out = fopen(outFile, "wb");

   fprintf(out, "import HParser.Grammar\nimport HParser.Generator\n\ngrammar = Grammar [\n");

   char* nt = readRule(NULL);
   while (nextChar() != -1) {
      if (nextChar() == '|') {
         readChar('|');
         fprintf(out, ",\n");
         readRule(nt);
      } else {
         readChar(';');
         free(nt);
         if (isalpha(nextChar())) {
            fprintf(out, ",\n");
            nt = readRule(NULL);
         } else {
            break;
         }
      }
   }

   fprintf(out, "\n   ]\n");

   fclose(in);
   fclose(out);

}