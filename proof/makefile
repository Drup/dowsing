.PHONY : all
all : coq latex

.PHONY : coq
coq :
	@ $(MAKE) -C coq

.PHONY : latex
latex :
	@ $(MAKE) -C latex

.PHONY : clean
clean :
	@ $(MAKE) -C coq clean
	@ $(MAKE) -C latex clean
