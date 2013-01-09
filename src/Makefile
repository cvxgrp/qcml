SRC = $(wildcard Expression/*.hs) $(wildcard Parser/*.hs) $(wildcard Atoms/*.hs) 
PROBSRC = scoop.hs $(SRC) $(wildcard CodeGenerator/*.hs)

.PHONY: all
all: scoop

scoop: $(PROBSRC)
	ghc --make -O $<

clean:
	rm -rf *.hi *.o Expression/*.hi Expression/*.o Parser/*.hi Parser/*.o CodeGenerator/*.hi CodeGenerator/*.o scoop
