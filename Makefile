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

.PHONY : bench
bench :
	@ $(DUNE) exec --release -- ./stat/main.exe stat --index=containers.db "int -> int -> int" "'a -> int -> unit" "'a -> 'a list -> bool" "('a * 'b) -> ('a -> 'c) -> ('b -> 'd) -> ('c * 'd)"
