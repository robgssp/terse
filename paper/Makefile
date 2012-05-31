.SUFFIXES:
.SUFFIXES: .tex .pdf .aux .bbl

paper-rough.pdf: paper-rough.tex
	pdflatex paper-rough.tex

paper-rough.aux: paper-rough.tex
	latex $<

paper-rough.bbl: paper.bib paper-rough.aux
	bibtex8 paper-rough.aux

view: paper-rough.pdf
	okular $<

clean:
	rm paper-rough.pdf paper-rough.aux paper-rough.bbl