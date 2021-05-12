.PHONY : all
all : agda latex

.PHONY : agda
agda :
	@ $(MAKE) -C agda

.PHONY : latex
latex :
	@ $(MAKE) -C latex

.PHONY : clean
clean :
	@ $(MAKE) -C agda clean
	@ $(MAKE) -C latex clean
