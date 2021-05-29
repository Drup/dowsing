RM := rm -f

COQC := coqc

.PHONY : all
all : TypWithoutUnit.vo

TypWithoutUnit.vo : Signature.vo Var.vo Vec.vo

%.vo : %.v
	@ $(COQC) $<

.PHONY : clean
clean :
	@ $(RM) *.glob *.vo{,k,s}
