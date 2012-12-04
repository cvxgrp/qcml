SRCDIR = src

.PHONY: all prog
all: prog

prog:
	cd $(SRCDIR); make

.PHONY: clean clean-prog
clean: clean-prog

clean-prog:
	cd $(SRCDIR); make clean
