AGDA := agda
RM := rm -f

.PHONY : all
all : ByHead.agdai ByTail.agdai

%.agdai : %.agda
	@ $(AGDA) $<

.PHONY : clean
clean :
	$(RM) *.agdai
