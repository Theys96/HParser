CC=gcc
CFLAGS=-O2
all: scanner parser
	${CC} ${CFLAGS} grammars.tab.c stringList.c -ll -lm -o HGrammar

scanner: grammars.lex
	flex -i grammars.lex
parser: grammars.y
	bison -d -v grammars.y
clean:
	rm -f lex.yy.c
	rm -f *.o
	rm -f *.tab.*
	rm -f *.output
	rm -f *~
