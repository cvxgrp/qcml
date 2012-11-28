SRCDIR = src
DOCDIR = doc
TALKDIR = talks

.PHONY: all doc prog talks
all: doc prog talks

doc:
	cd $(DOCDIR); make

prog:
	cd $(SRCDIR); make

talks:
	cd $(TALKDIR); make

.PHONY: clean clean-doc clean-prog
clean: clean-doc clean-prog clean-talk

clean-doc:
	cd $(DOCDIR); make clean

clean-prog:
	cd $(SRCDIR); make clean

clean-talk:
	cd $(TALKDIR); make clean
