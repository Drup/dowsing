RM := rm -f
PDFLATEX := pdflatex -shell-escape

.PHONY : all
all : mlworkshop.pdf

%.pdf : %.tex
	$(PDFLATEX) $<

.PHONY : clean
clean :
	$(RM) *.aux *.log
