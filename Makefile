SRCDIR = src
DOCDIR = doc

.PHONY: all doc prog
all: doc prog

doc:
	cd $(DOCDIR); make

prog:
	cd $(SRCDIR); make

.PHONY: clean clean-doc clean-prog
clean: clean-doc clean-prog

clean-doc:
	cd $(DOCDIR); make clean

clean-prog:
	cd $(SRCDIR); make clean
