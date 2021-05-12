RM := rm -f
PDFLATEX := pdflatex

.PHONY : all
all : proof.pdf

%.pdf : %.tex
	$(PDFLATEX) $<

.PHONY : clean
clean :
	$(RM) *.aux *.log
