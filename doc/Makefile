DEPS = userguide.tex userguide.bib

userguide.pdf: $(DEPS)
	latex $<;
	bibtex $(<:.tex=);
	latex $<;
	latex $<;
	dvipdf $(<:.tex=);

clean:
	rm -rf *.pdf *.aux *.dvi *.log *.out *.bbl *.blg *.toc
