RM := rm -f
PDFLATEX := pdflatex -shell-escape
BIBER := biber

.PHONY : all
all : $(TARGET).pdf

%.pdf : %.tex %.bib
	$(PDFLATEX) $<
	$(BIBER) $*
	$(PDFLATEX) $<
	$(PDFLATEX) $<

%.pdf : %.tex
	$(PDFLATEX) $<

.PHONY : clean
clean :
	@ $(RM) -r *.{aux,log,nav,out,snm,synctex.gz,toc,vrb,bbl,bcf,blg,run.xml} _minted-*

.PHONY : distclean
distclean : clean
	@ $(RM) *.pdf
