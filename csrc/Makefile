# Makefile -- relies on implicit rules
# compiles the SCOOP parser
PROG=scoop
LEX=flex
LDFLAGS=-ll
CFLAGS=-ansi
 
$(PROG): $(PROG).o
$(PROG).o: $(PROG).l

.PHONY:clean
clean:
	rm $(PROG).o 

