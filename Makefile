all:
	make install
	make build
	make test
	make clean

install:
	cabal install HUnit

build: 
	cd HGrammar && make

test:
	ghc Test.hs -o test

clean:
	cd HGrammar && make clean
	rm -f *~ && rm -f *.hi && rm -f *.o
	cd HParser && rm -f *~ && rm -f *.hi && rm -f *.o
	
