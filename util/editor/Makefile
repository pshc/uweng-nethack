
all:
	ghc -lcurses --make -o editor Editor.hs
	./editor

%.o: %.hs
	ghc -o $@ -c $<

clean:
	rm -f -- editor *.hi *.o
