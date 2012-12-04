SRC = $(wildcard Expression/*.hs) $(wildcard Parser/*.hs) $(wildcard Atoms/*.hs) 
PROBSRC = SCOOP.hs $(SRC) $(wildcard CodeGenerator/*.hs)

.PHONY: all
all: scoop

scoop: $(PROBSRC)
	ghc --make $<

clean:
	rm -rf *.hi *.o Expression/*.hi Expression/*.o Parser/*.hi Parser/*.o CodeGenerator/*.hi CodeGenerator/*.o scoop
