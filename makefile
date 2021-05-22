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

.PHONY : proof
proof :
	@ $(MAKE) -C proof

.PHONY : tex
tex :
	@ $(MAKE) -C tex

.PHONY : clean
clean :
	@ $(DUNE) clean
	@ $(MAKE) -C proof clean
	@ $(MAKE) -C tex clean

.PHONY : distclean
distclean :
	@ $(MAKE) -C tex distclean
