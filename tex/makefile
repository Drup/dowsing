subs := mlworkshop prez_unifAC proof proof2 rapport
mk_sub = $(MAKE) -C $1 -f ../tex.mk TARGET=$1

.PHONY : all
all : $(subs)

.PHONY : $(subs)
$(subs) :
	@ $(call mk_sub,$@)

.PHONY : clean
clean : $(addprefix clean_,$(subs))

clean_% :
	@ $(call mk_sub,$*) clean

.PHONY : distclean
distclean : $(addprefix distclean_,$(subs))

distclean_% :
	@ $(call mk_sub,$*) distclean
