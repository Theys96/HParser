build: 
	cd HGrammar && make
	ghc Test.hs -o test

clean:
	cd HGrammar && make clean
	rm -f *~
	rm -f *.hi
	rm -f *.o

