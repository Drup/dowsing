SRCTEX=$(wildcard *.tex)
FIGS=$(wildcard figs/*.pdf) $(wildcard figs/*.png)


MAIN=proof

all:  $(MAIN).pdf 

%.pdf: %.tex # *.bib $(FIGS)
	rubber --pdf $<

clean:
	rm -f *.o *.aux *.log *.out *.bbl *.toc $(MAIN).pdf *.blg *.dvi *~ *.fls *_latexmk *.snm *.nav *.spl *.ilg *.ind *.idx paper_eval.pdf
