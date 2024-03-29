DUNE := dune

.PHONY : build
all : build

.PHONY : build
build :
	@ $(DUNE) build @install

.PHONY : test
test :
	@ $(DUNE) runtest --no-buffer

.PHONY : doc
doc :
	@ $(DUNE) build @doc

.PHONY : watch
watch :
	@ $(DUNE) build @all -w

.PHONY : clean
clean :
	@ $(DUNE) clean
