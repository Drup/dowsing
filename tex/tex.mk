RM := rm -f
PDFLATEX := pdflatex -shell-escape

.PHONY : all
all : $(TARGET).pdf

%.pdf : %.tex
	$(PDFLATEX) $<

.PHONY : clean
clean :
	$(RM) -r *.{aux,log,nav,out,snm,synctex.gz,toc,vrb,bbl,bcf,blg,run.xml} _minted-*
